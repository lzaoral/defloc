{-# LANGUAGE TupleSections #-}

{-
    Copyright (C) 2021 Red Hat, Inc.

    This file is part of defloc.

    defloc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    defloc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main (main) where

import ShellCheck.AST
import ShellCheck.Interface
import ShellCheck.Parser

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError, liftIO)

import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromJust, isJust, isNothing)

import qualified Data.Map  as M
import qualified Data.List as L

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import Text.Regex.PCRE ((=~))


findFuncTokenIds :: String -> Token -> [(Id, String)]
findFuncTokenIds func (OuterToken tokID it) = case it of
    Inner_TA_Assignment _ t1 t2 -> ff t1 ++ ff t2
    Inner_TA_Binary _ t1 t2 -> ff t1 ++ ff t2
    Inner_TA_Expansion ts -> concatMap ff ts
    Inner_TA_Sequence ts -> concatMap ff ts
    Inner_TA_Trinary t1 t2 t3 -> ff t1 ++ ff t2 ++ ff t3
    Inner_TA_Unary _ t -> ff t
    Inner_TA_Variable _ ts -> concatMap ff ts
    Inner_TC_And _ _ t1 t2 -> ff t1 ++ ff t2
    Inner_TC_Binary _ _ t1 t2 -> ff t1 ++ ff t2
    Inner_TC_Group _ t -> ff t
    Inner_TC_Nullary _ t  -> ff t
    Inner_TC_Or _ _ t1 t2 -> ff t1 ++ ff t2
    Inner_TC_Unary _ _ t -> ff t
    Inner_T_AndIf t1 t2 -> ff t1 ++ ff t2
    Inner_T_Arithmetic t -> ff t
    Inner_T_Array ts -> concatMap ff ts
    Inner_T_IndexedElement ts t -> concatMap ff ts ++ ff t
    Inner_T_Assignment _ _ ts t -> concatMap ff ts ++ ff t
    Inner_T_Backgrounded t -> ff t
    Inner_T_Backticked ts -> concatMap ff ts
    Inner_T_Banged t -> ff t
    Inner_T_BraceExpansion ts -> concatMap ff ts
    Inner_T_BraceGroup ts -> concatMap ff ts
    Inner_T_CaseExpression t ts -> ff t ++
        concatMap (\(_, ts1, ts2) -> concatMap ff ts1 ++ concatMap ff ts2) ts
    Inner_T_Condition _ t -> ff t
    Inner_T_DollarArithmetic t -> ff t
    Inner_T_DollarBraced _ t -> ff t
    Inner_T_DollarBracket t -> ff t
    Inner_T_DollarDoubleQuoted ts -> concatMap ff ts
    Inner_T_DollarExpansion ts -> concatMap ff ts
    Inner_T_DollarBraceCommandExpansion ts -> concatMap ff ts
    Inner_T_DoubleQuoted ts -> concatMap ff ts
    Inner_T_Extglob _ ts -> concatMap ff ts
    Inner_T_FdRedirect _ t -> ff t
    Inner_T_ForArithmetic t1 t2 t3 ts -> ff t1 ++ ff t2 ++ ff t3 ++
        concatMap ff ts
    Inner_T_ForIn _ ts1 ts2 -> concatMap ff ts1 ++ concatMap ff ts2
    Inner_T_Function _ _ name t -> if name =~ func
                                      then (tokID, name) : ff t
                                      else ff t
    Inner_T_HereDoc _ _ _ ts -> concatMap ff ts
    Inner_T_HereString t -> ff t
    Inner_T_IfExpression ts1 ts2 -> concatMap ff ts2 ++
        concatMap (\(ts3, ts4) -> concatMap ff ts3 ++ concatMap ff ts4) ts1
    Inner_T_IoFile t1 t2 -> ff t1 ++ ff t2
    Inner_T_IoDuplicate t _ -> ff t
    Inner_T_NormalWord ts -> concatMap ff ts
    Inner_T_OrIf t1 t2 -> ff t1 ++ ff t2
    Inner_T_Pipeline ts1 ts2 -> concatMap ff ts1 ++ concatMap ff ts2
    Inner_T_ProcSub _ ts -> concatMap ff ts
    Inner_T_Redirecting ts t -> concatMap ff ts ++ ff t
    Inner_T_Script t ts -> ff t ++ concatMap ff ts
    Inner_T_SelectIn _ ts1 ts2 -> concatMap ff ts1 ++ concatMap ff ts2
    Inner_T_SimpleCommand ts1 ts2 -> concatMap ff ts1 ++ concatMap ff ts2
    Inner_T_Subshell ts -> concatMap ff ts
    Inner_T_UntilExpression ts1 ts2 -> concatMap ff ts1 ++ concatMap ff ts2
    Inner_T_WhileExpression ts1 ts2 -> concatMap ff ts1 ++ concatMap ff ts2
    Inner_T_Annotation _ t -> ff t
    Inner_T_CoProc _ t -> ff t
    Inner_T_CoProcBody t -> ff t
    Inner_T_Include t -> ff t
    Inner_T_SourceCommand t1 t2 -> ff t1 ++ ff t2
    Inner_T_BatsTest t1 t2 -> ff t1 ++ ff t2
    _ -> []

    where ff = findFuncTokenIds func


type ProcessMonad = ExceptT String IO

processReport :: String -> (FilePath, ParseResult) -> ProcessMonad ()
processReport func (file, result) = do
    -- ShellCheck does not export the ParseResult data constructor
    let comments    = prComments result
        positionMap = prTokenPositions result
        root        = prRoot result

    when (isNothing root) $ throwError $
        file ++ ": Parsing failed (no AST generated)"

    let com = L.find ((== ErrorC) . cSeverity . pcComment) comments
    when (isJust com) $ throwError $
        file ++ ": Parsing failed (incomplete AST generated)"

    let ids = findFuncTokenIds func $ fromJust root
    mapM_ (\(i, f) -> printLoc f $ fromJust $ M.lookup i positionMap) ids

    where printLoc f          = liftIO . putStrLn . strPos f
          strPos f (from, to) = concat [posFile from, ":", f, ":", strLoc from,
                                        "-", strLoc to]
          strLoc pos          = concat [show (posLine pos), ":",
                                        show (posColumn pos)]


processFiles :: String -> [(FilePath, String)] -> IO ()
processFiles func = mapM_ process
    where process x = runExceptT (processReport func $ parse x) >>= handle
          parse (file, contents) = (file, runIdentity $ parseScript intf spec)
              where intf = mockedSystemInterface []
                    spec = newParseSpec { psFilename = file,
                                          psScript   = contents }
          handle (Left e) = hPutStrLn stderr e
          handle _ = return ()


-- Argument parsing
processOpts :: [String] -> IO (String, [FilePath])
processOpts (func:scripts@(_:_)) = return (func, scripts)
processOpts _ = ioError $ userError $ "Incorrect number of arguments.\n" ++
                                      "USAGE: defloc FUNCTION [SCRIPTS]\n"


main :: IO ()
main = do
    (func, scripts) <- getArgs >>= processOpts
    mapM readWithName scripts >>= processFiles func
    where readWithName x = (x,) <$> readFile x

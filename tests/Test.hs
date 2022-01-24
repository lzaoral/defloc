{-
    Copyright (C) 2021-2022 Red Hat, Inc.

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

import Data.Either (isRight)
import Data.List (intercalate)
import Defloc (parse, processReport)

import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>))

import Test.HUnit


data TestInfo = TestInfo { function :: String
                         , file     :: FilePath
                         , isOK     :: Bool
                         , expected :: String
                         }


strJoin :: [String] -> String
strJoin = intercalate "\n"


makeTest :: TestInfo -> Test
makeTest (TestInfo func f ok ex) = TestCase $ do
    src <- readFile $ "tests" </> f
    let res = processReport func $ parse (f, src)

    isRight res @=? ok
    case res of
        Left  x  -> ex @=? x
        Right xs -> ex @=? strJoin xs


tests :: Test
tests = TestList [
      "simple definitions" ~: makeTest TestInfo { function = "foo"
                                                , file     = "simple.sh"
                                                , isOK     = True
                                                , expected = strJoin [
                                                      "simple.sh:foo:3:1-5:2",
                                                      "simple.sh:foo:7:1-9:2",
                                                      "simple.sh:foo1:11:1-13:2"
                                                  ]
                                                }
    ,  "simple regex"      ~: makeTest TestInfo { function = "foo\\d"
                                                , file     = "simple.sh"
                                                , isOK     = True
                                                , expected = strJoin [
                                                      "simple.sh:foo1:11:1-13:2"
                                                  ]
                                                }
    ,  "non-fatal error"   ~: makeTest TestInfo { function = "foo"
                                                , file     = "error.sh"
                                                , isOK     = True
                                                , expected = strJoin [
                                                      "error.sh:foo:6:1-8:2"
                                                  ]
                                                }
    , "invalid syntax"     ~: makeTest TestInfo { function = "foo"
                                                , file     = "invalid.sh"
                                                , isOK     = False
                                                , expected = strJoin [
                              "invalid.sh: Parsing failed (no AST generated)"
                                                  ]
                                                }
    ]

main :: IO ()
main = do
    counts2 <- runTestTT tests
    if errors counts2 + failures counts2 == 0
       then exitSuccess
       else exitFailure

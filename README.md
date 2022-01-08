# defloc

[![Haskell CI](https://github.com/lzaoral/defloc/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/lzaoral/defloc/actions/workflows/haskell-ci.yml)
[![Copr build status](https://copr.fedorainfracloud.org/coprs/lzaoral/defloc/package/defloc/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/lzaoral/defloc/)

Simple tool based on ShellCheck's parser that can be used to find definitions
of functions in shell scripts.

## Installation

### Manual

Make sure you have `cabal` installed (at least v3 recommended).

1. Clone this repository.
2. Run `cabal update`.
3. Run `cabal install --installdir=<path>`.
4. `defloc` binary will be present in `<path>` specified in the previous step.
5. ...
6. Profit!

### Fedora

You can setup [`defloc`'s COPR repository](https://copr.fedorainfracloud.org/coprs/lzaoral/defloc/)
using the following commands.   Make sure you already have `dnf-plugins-core`
installed.  Otherwise, `dnf` will not recognise the `copr` command.
```console
# dnf copr enable lzaoral/defloc
# dnf install defloc
```

## Usage

```shell
defloc function [scripts]
```

## Examples

```console
$ cat test.sh
#!/usr/bin/env sh

foo() {
    :
}

foo() (
    :
)

function foo1() {
    :
}

$ defloc foo test.sh
test.sh:foo:3:1-5:2
test.sh:foo:7:1-9:2
test.sh:foo1:11:1-13:2

# You can use regular expressions as well.
$ defloc 'foo\d' test.sh
test.sh:foo1:11:1-13:2

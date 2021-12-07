# defloc

[![Haskell CI](https://github.com/lzaoral/defloc/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/lzaoral/defloc/actions/workflows/haskell-ci.yml)
[![Copr build status](https://copr.fedorainfracloud.org/coprs/lzaoral/defloc/package/defloc/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/lzaoral/defloc/)

Simple tool based on ShellCheck's parser that can be used to find definitions
of functions in shell scripts.

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

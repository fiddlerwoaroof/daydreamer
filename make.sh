#!/usr/bin/env bash
export ASDF_OUTPUT_TRANSLATIONS="/:" CC="clang"

env

sbcl --no-userinit --disable-debugger --load /root/quicklisp/setup.lisp --load build.lisp

./daydreamer --self-test

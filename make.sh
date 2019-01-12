#!/usr/bin/env bash
export ASDF_OUTPUT_TRANSLATIONS="/:" CC="clang"

env

sbcl --no-userinit --disable-debugger --load ~/sbcl/quicklisp/setup.lisp --load build.lisp
ls ~/sbcl/quicklisp/local-projects/daydreamer/
./daydreamer --self-test

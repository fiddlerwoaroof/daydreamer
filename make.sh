#!/usr/bin/env bash
export ASDF_OUTPUT_TRANSLATIONS="/:" SBCL_HOME="$HOME/sbcl/lib/sbcl/" CC="clang"

ls -R ~/sbcl
~/sbcl/bin/sbcl --no-userinit --disable-debugger --load ~/sbcl/quicklisp/setup.lisp --load build.lisp
ls ~/sbcl/quicklisp/local-projects/daydreamer/
./daydreamer --self-test

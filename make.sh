#!/usr/bin/env bash
~/sbcl/bin/sbcl --no-userinit --disable-debugger --load ~/sbcl/quicklisp/setup.lisp --load build.lisp
ls ~/sbcl/quicklisp/local-projects/daydreamer/
./daydreamer --self-test
rm -rf ~/sbcl/quicklisp/local-projects/*

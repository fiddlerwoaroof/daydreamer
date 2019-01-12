#!/usr/bin/env bash
cd $HOME

pwd
git clone --depth=1 https://gitlab.com/fiddlerwoaroof/osicat.git ~/quicklisp/local-projects/osicat
git clone --depth=1 https://gitlab.com/fiddlerwoaroof/cffi.git ~/quicklisp/local-projects/cffi

git clone --depth=1 https://github.com/fiddlerwoaroof/fwoar.lisputils.git ~/quicklisp/local-projects/fwoar.lisputils/
git clone --depth=1 https://github.com/fiddlerwoaroof/data-lens.git ~/quicklisp/local-projects/data-lens/
git clone --depth=1 https://github.com/pokepay/aws-sdk-lisp.git ~/quicklisp/local-projects/aws-sdk-lisp && pushd ~/quicklisp/local-projects/aws-sdk-lisp && git checkout 6d1f66e && popd

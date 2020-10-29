#!/usr/bin/env bash

set -eu -o pipefail

cd "$HOME" 

pwd

git clone --depth=1 https://gitlab.com/fiddlerwoaroof/osicat.git "$HOME"/quicklisp/local-projects/osicat
git clone --depth=1 https://gitlab.com/fiddlerwoaroof/cffi.git "$HOME"/quicklisp/local-projects/cffi

git clone --depth=1 https://github.com/fiddlerwoaroof/fwoar.lisputils.git "$HOME"/quicklisp/local-projects/fwoar.lisputils/
git clone --depth=1 https://github.com/fiddlerwoaroof/data-lens.git "$HOME"/quicklisp/local-projects/data-lens/
git clone --depth=1 https://github.com/fiddlerwoaroof/aws-sdk-lisp.git "$HOME"/quicklisp/local-projects/aws-sdk-lisp

pushd "$HOME"/quicklisp/local-projects/aws-sdk-lisp
git checkout my-patches
popd

ln -s /root/project/ "$HOME"/quicklisp/local-projects/daydreamer

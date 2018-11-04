#!/usr/bin/env bash
cd $HOME

wget -q-O- https://srv2.elangley.org/~edwlan/sbcl-static.tgz|tar vzx 

git clone --depth=1 https://gitlab.com/fiddlerwoaroof/osicat.git ~/sbcl/quicklisp/local-projects/osicat
git clone --depth=1 https://gitlab.com/fiddlerwoaroof/cffi.git ~/sbcl/quicklisp/local-projects/cffi

git clone --depth=1 https://github.com/fiddlerwoaroof/daydreamer.git ~/sbcl/quicklisp/local-projects/daydreamer/
git clone --depth=1 https://github.com/fiddlerwoaroof/fwoar.lisputils.git ~/sbcl/quicklisp/local-projects/fwoar.lisputils/
git clone --depth=1 https://github.com/fiddlerwoaroof/data-lens.git ~/sbcl/quicklisp/local-projects/data-lens/
git clone --depth=1 https://github.com/pokepay/aws-sdk-lisp.git ~/sbcl/quicklisp/local-projects/aws-sdk-lisp && pushd ~/sbcl/quicklisp/local-projects/aws-sdk-lisp && git checkout 6d1f66e && popd

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

#+sbcl
(sb-posix:setenv "CC" "clang" 1)

(defsystem :daydreamer
    :description ""
    :author "Ed L <gh@elangley.org>"
    :license "MIT"
    :depends-on (#:alexandria
                 #:uiop
                 #:serapeum
                 #:fwoar.lisputils
                 #:net.didierverna.clon
                 #:cl-base64
                 #:local-time
                 #:local-time-duration
                 #:aws-sdk/services/cloudformation
                 #:aws-sdk/services/monitoring
                 #:aws-sdk/services/elasticmapreduce
                 #:should-test
                 #:yason
                 #:hunchentoot
                 #:data-lens
                 )
    :serial t
    :components ((:file "aws-result")
                 (:file "main")
                 (:file "cli")))

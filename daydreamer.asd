;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

#+sbcl
(sb-posix:setenv "CC" "clang" 1)

(defsystem :daydreamer
  :description ""
  :author "Ed L <gh@elangley.org>"
  :license "MIT"
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:fwoar.lisputils
               #:net.didierverna.clon
               #:cl-base64
               #:local-time
               #:osicat
               #:local-time-duration
               #:aws-sdk/services/cloudformation
               #:aws-sdk/services/monitoring
               #:aws-sdk/services/elasticmapreduce
               #:should-test
               #:yason
               #:hunchentoot
               #:data-lens
               #:cffi)
  :serial t
  :components ((:file "aws-result")
               (:file "main")
               (:file "cli"))

  :entry-point "daydreamer.cli::main"
  :output-files (cffi-toolchain:static-program-op (o c)
                                                  (format t "~&*dpd* ~s~%%" (merge-pathnames "daydreamer"
                                                                                             *default-pathname-defaults*))
                                                  (list
                                                   (merge-pathnames "daydreamer"
                                                                    *default-pathname-defaults*))))

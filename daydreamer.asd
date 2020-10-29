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
               #:fwoar-lisputils
               #:cl-base64
               #:local-time
               #:local-time-duration
               #:closer-mop
               #:aws-sdk                 
               #:aws-sdk/services/cloudformation
               #:aws-sdk/services/monitoring
               #:aws-sdk/services/elasticmapreduce
               #:yason
               #:hunchentoot
               #:data-lens
               #:cffi
               (:feature (:not :lispworks)
                         #:net.didierverna.clon)
               (:feature (:not :lispworks)
                         #:should-test))
  :serial t
  :components ((:file "aws-result")
               (:file "main")
               (:file "cli" :if-feature (:not :lispworks)))

  :entry-point "daydreamer.cli::main"
  #-lispworks 
  :output-files
  #-lispworks
  (cffi-toolchain:static-program-op (o c)
                                    (format t "~&*dpd* ~s~%%" (merge-pathnames "daydreamer"
                                                                               *default-pathname-defaults*))
                                    (list
                                     (merge-pathnames "daydreamer"
                                                      *default-pathname-defaults*))))

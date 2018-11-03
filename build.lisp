(defpackage :fwoar.build
  (:use :cl)
  (:export make))
(in-package :fwoar.build)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro stepwise (&body body)
  `(progn
     ,@(mapcar (lambda (x)
                 `(eval-always
                    ,@x))
               body)))

;; #.(progn
;;     (load "~/quicklisp/setup.lisp")
;;     nil)

(stepwise
  ((defun load-compile (pn)
     (load (compile-file pn))))

  ((ql:quickload :cffi-grovel))
  ((princ
    (mapcar 'load-compile
            (remove #\.
                    (directory (merge-pathnames "*.asd"
                                                *load-pathname*)) 
                    :key 'pathname-name 
                    :test 'alexandria:starts-with)))
   (terpri))

  ((ql:quickload :daydreamer))

  ((asdf:operate :static-program-op :daydreamer))
  #+nil
  ((asdf/driver:symbol-call :daydreamer.cli :dump))
  
  (sb-ext:quit))

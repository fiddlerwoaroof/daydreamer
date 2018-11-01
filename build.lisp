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



(define-symbol-macro *my-pathname* 
    (or *compile-file-pathname*
        *load-pathname*))

(stepwise
  ((defun load-compile (pn)
     (load (compile-file pn))))

  ((princ
    (mapcar 'load-compile
            (directory (merge-pathnames "*.asd"
                                        *my-pathname*))))
   (terpri))

  ((ql:quickload :daydreamer))

  ((asdf/driver:symbol-call :daydreamer.cli :dump)))

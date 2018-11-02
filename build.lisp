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



(stepwise
  ((defun tr (v)
     (prog1 v
       (format t "~&tracing: ~s~%" v))))

  ((defun load-compile (pn)
     (load (compile-file pn))))

  ((princ
    (mapcar 'load-compile
            (directory (merge-pathnames "*.asd"
                                        *load-pathname*))))
   (terpri))

  ((ql:quickload :daydreamer/release))

  ((asdf:operate :static-program-op :daydreamer/release))
  #+nil
  ((asdf/driver:symbol-call :daydreamer.cli :dump)))

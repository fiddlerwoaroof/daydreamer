;; MIT license Edward Langley (c) 2018

(eval-when (:execute :load-toplevel :compile-toplevel)
  (net.didierverna.clon:nickname-package))

(defpackage :cloud-watcher.main
  (:use :cl :fw.lu :alexandria :st :cloud-watcher.aws-result)
  (:export main dump
           #:stack-parameters
           #:stack-outputs
           #:stack-for-name
           #:watch-stack))

(in-package :cloud-watcher.main)

(define-condition invalid-result (error)
  ())

(defun stack-for-name (name)
  (let* ((aws-result (car (extract-list (cdar (aws/cloudformation:describe-stacks :stack-name name)))))
         (the-stack (extract-stack aws-result)))
    the-stack))

(defun print-kvs (formatter stream data)
  (mapcar (destructuring-lambda (((_ k) (__ v)))
            (declare (ignore _ __))
            (funcall formatter stream k v))
          data))

(defun stack-outputs (the-stack)
  (print-kvs (tagged-kv-formatter "OUTPUT") t
             (outputs the-stack)))

(defun stack-parameters (the-stack)
  (print-kvs (tagged-kv-formatter "PARAMETERS") t
             (parameters the-stack)))

(defun lt-format (a b &key &allow-other-keys)
  (local-time:format-timestring a b))

(defgeneric duration-of (timeline)
  (:method ((timeline timeline))
    (local-time-duration:timestamp-difference (end-date-time timeline)
                                              (start-date-time timeline))))

(defun watch-stack (name)
  (format t "~&Watching ~s~2%" name)
  (let ((done? nil)
        (old-status nil))
    (loop until done?
       for the-stack = (stack-for-name name)
       do
         (unless old-status
           (format t "~& PARAMETERS ~%============~%")
           (stack-parameters the-stack)
           (format t "~&============~2%"))

         (unless (eql old-status (stack-status the-stack))
           (format t "~&STATUS ~a~%" (stack-status the-stack))
           (setf old-status (stack-status the-stack)))

         (if (ends-with-subseq "COMPLETE" (symbol-name (stack-status the-stack)))
             (progn
               (format t "~2& OUTPUTS ~%=========~%")
               (stack-outputs the-stack)
               (format t "~&=========~%")
               (return)))
         (sleep 5)))
  (fresh-line))

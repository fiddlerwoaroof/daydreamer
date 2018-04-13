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

(defun every-five-seconds (cb args &optional (delay-function #'sleep))
  (loop
     while (apply cb args)
     do (funcall delay-function 5)))

(deftest every-five-seconds ()
  (let ((counter 0)
        delay)
    (flet ((fake-delay (num)
             (incf counter)
             (setf delay num))
           (work ()
             (< counter 10)))
      (every-five-seconds #'work () #'fake-delay)
      (should be = 10 counter)
      (should be = 5 delay))))

(defclass stack-watcher ()
  ())

(defun watch-stack (name)
  (format t "~&Watching ~s~2%" name)
  (block nil
    (let ((old-status nil))
      (every-five-seconds
       (lambda ()
         (let* ((the-stack (stack-for-name name))
                (current-status (stack-status the-stack)))
           (unless old-status
             (format t "~& PARAMETERS ~%============~%")
             (stack-parameters the-stack)
             (format t "~&============~2%"))

           (unless (eql old-status current-status)
             (format t "~&STATUS ~a~%" current-status)
             (setf old-status current-status))

           (if (ends-with-subseq "COMPLETE" (symbol-name current-status))
               (progn
                 (format t "~2& OUTPUTS ~%=========~%")
                 (stack-outputs the-stack)
                 (format t "~&=========~%")
                 nil)
               t)))
       ())))
  (fresh-line))


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
     for (continue? . next-args) = (multiple-value-list (apply cb args)) then (multiple-value-list (apply cb next-args))
     while continue?
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
      (should be = 5 delay)))

  (let (delay
        (count 0)
        (counters ()))
    (flet ((fake-delay (num)
             (setf delay num))
           (work (counter)
             ;; temporary for initial red...
             (if (> count 10)
                 (throw 'quit nil)
                 (incf count))

             (push counter counters)
             (values (< counter 10)
                     (1+ counter))))
      (catch 'quit
        (every-five-seconds #'work '(0) #'fake-delay))
      (should be equal
              (alexandria:iota 11 :start 10 :step -1)
              counters)
      (should be = 5 delay))))

(defun parameter-block (the-stack)
  (format t "~& PARAMETERS ~%============~%")
  (stack-parameters the-stack)
  (format t "~&============~2%"))

(defun output-block (the-stack)
  (format t "~2& OUTPUTS ~%=========~%")
  (stack-outputs the-stack)
  (format t "~&=========~%")
  (values))

(defun stack-info (the-stack)
  (lambda (old-status)
    (unless old-status
      (parameter-block the-stack))

    (let ((current-status (stack-status the-stack)))
      (unless (eql old-status current-status)
        (format t "~&STATUS ~a~%" current-status)
        (setf old-status current-status))

      (values (if (ends-with-subseq "COMPLETE" (symbol-name current-status))
                  (output-block the-stack)
                  t)
              current-status))))

(defun watch-stack (name)
  (let ((the-stack (stack-for-name name)))
    (format t "~&Watching ~s~2%" name)
    (every-five-seconds (stack-info the-stack)
                        (list nil))
    (fresh-line)))


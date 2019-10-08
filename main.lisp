;; MIT license Edward Langley (c) 2018

(defpackage :daydreamer.main
  (:use :cl :fw.lu :alexandria :st :daydreamer.aws-result)
  (:export main dump
           #:stack-parameters
           #:stack-outputs
           #:stack-for-name
           #:watch-stack
           #:stack-info))

(in-package :daydreamer.main)

(define-condition invalid-result (error)
  ())

(defun stack-for-name (name)
  (let* ((aws-result (car (extract-list (cdar (aws/cloudformation:describe-stacks :stack-name name)))))
         (the-stack (extract-stack aws-result)))
    the-stack))

(defun print-kvs (prefix formatter stream data)
  (let ((key-key (format nil "~aKey" prefix))
        (value-key (format nil "~aValue" prefix)))
    (mapcar (lambda (inp)
              (let ((k (cadr (assoc key-key inp :test #'equal)))
                    (v (cadr (assoc value-key inp :test #'equal))))
                (format stream formatter k v)))
            data)))

(defun stack-outputs (the-stack)
  (print-kvs "Output"
             (tagged-kv-formatter "OUTPUT") t
             (outputs the-stack)))

(defun stack-parameters (the-stack)
  (print-kvs "Parameter"
             (tagged-kv-formatter "PARAMETERS") t
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

(defclass stack-formatter ()
  ((%stack :initarg :stack :accessor stack)
   (%old-status :initarg :old-status :accessor old-status :initform nil)))

(defmethod stack-status ((stack-formatter stack-formatter))
  (when (slot-boundp stack-formatter '%stack)
    (stack-status (stack stack-formatter))))

(defmethod parameters ((stack-formatter stack-formatter))
  (when (slot-boundp stack-formatter '%stack)
    (parameters (stack stack-formatter))))

(defmethod outputs ((stack-formatter stack-formatter))
  (when (slot-boundp stack-formatter '%stack)
    (outputs (stack stack-formatter))))

(defmethod (setf stack) :before (new-value (object stack-formatter))
  (setf (old-status object) (stack-status object)))

(defgeneric refresh (stack-formatter)
  (:method ((stack daydreamer.aws-result:stack))
    (stack-for-name (stack-name stack))) 
  (:method ((stack-formatter string))
    (make-instance 'stack-formatter :stack (stack-for-name stack-formatter)))
  (:method ((stack-formatter stack-formatter))
    (setf (stack stack-formatter) (refresh (stack stack-formatter)))
    stack-formatter))

(defmethod old-status ((stack daydreamer.aws-result:stack))
  nil)

(defun stack-info (the-stack)
  (with-accessors ((old-status old-status)) the-stack
    (let* ((current-status (stack-status the-stack)))
      (unless old-status
        (parameter-block the-stack))

      (unless (equal old-status current-status)
        (format t "~&STATUS ~a~%" current-status))

      (if (ends-with-subseq "COMPLETE" (symbol-name current-status))
          (output-block the-stack)
          t))))

(defmacro refreshing (cb)
  `(lambda (thing)
     (let ((refreshed-thing (refresh thing)))
       (values (,cb refreshed-thing)
               refreshed-thing))))

(defun watch-stack (name)
  (format t "~&Watching ~s~2%" name)
  (every-five-seconds (refreshing stack-info)
                      (list name))
  (fresh-line))


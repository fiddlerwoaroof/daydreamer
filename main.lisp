;; MIT license Edward Langley (c) 2018

(defpackage :cloud-watcher.cli
  (:use :cl :net.didierverna.clon)
  (:export options
           #:*cloud-watcher-synopsis*))

(defpackage :cloud-watcher.main
  (:import-from :cloud-watcher.cli #:*cloud-watcher-synopsis*)
  (:use :cl :fw.lu :alexandria :st)
  (:export main dump))

(in-package :cloud-watcher.cli)

(defparameter *cloud-watcher-synopsis*
  (defsynopsis (:postfix "ARGS...")
    (group (:header "actions")
           (flag :short-name "p" :long-name "parameters" :description "show stack parameters")
           (flag :short-name "o" :long-name "outputs" :description "show stack outputs")
           (flag :short-name "w" :long-name "watch" :description "watch a cloudformation stack until it's done processing")
           (flag :short-name "s" :long-name "start")
           (flag :short-name "u" :long-name "update"))
    (group (:header "misc")
           (flag :long-name "help"))))

(in-package :cloud-watcher.main)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (net.didierverna.clon:nickname-package))

(define-condition invalid-result (error)
  ())

(defun extract-list (aws-result)
  (mapcar (destructuring-lambda ((list-item-marker . items))
            (if (string= list-item-marker "member")
                items
                (error 'invalid-result)))
          aws-result))

(defclass stack ()
  ((%outputs :initarg :outputs :reader outputs)
   (%capabilities :initarg :capabilities :reader capabilities)
   (%creation-time :initarg :creation-time :reader creation-time)
   (%notification-arns :initarg :notification-arns :reader notification-arns)
   (%stack-id :initarg :stack-id :reader stack-id)
   (%stack-name :initarg :stack-name :reader stack-name)
   (%description :initarg :description :reader description)
   (%stack-status :initarg :stack-status :reader stack-status)
   (%disable-rollback :initarg :disable-rollback :reader disable-rollback)
   (%tags :initarg :tags :reader tags)
   (%rollback-configuration :initarg :rollback-configuration :reader rollback-configuration)
   (%drift-information :initarg :drift-information :reader drift-information)
   (%enable-termination-protection :initarg :enable-termination-protection :reader enable-termination-protection)
   (%parameters :initarg :parameters :reader parameters)))

(defun find-all-indices (pred str &optional accum (start (or (car accum) 0)))
  (check-type pred function)
  (check-type str string)
  (check-type start fixnum)

  (tagbody start-loop
     (if (= start (length str))
         (return-from find-all-indices (nreverse accum))
         (let ((pos (position-if pred str :start start)))
           (if pos
               (progn (psetf accum (cons pos accum)
                             start (1+ pos))
                      (go start-loop))
               (return-from find-all-indices (nreverse accum)))))))

(defun decamelize (v)
  (let* ((indices (or (find-all-indices #'upper-case-p v) '(0)))
         (parts (mapcar (serapeum:op (string-downcase (subseq v _ _)))
                        (if (= (car indices) 0) indices (cons 0 indices))
                        (if (/= (car indices) 0) indices (append (cdr indices)
                                                                 (list (length v)))))))
    (serapeum:string-join parts "-")))

(deftest decamelize ()
  (should be equal "a" (decamelize "A"))
  (should be equal "a" (decamelize "a"))
  (should be equal "outputs" (decamelize "Outputs"))
  (should be equal "outputs-outputs" (decamelize "OutputsOutputs"))
  (should be equal "a-b-c" (decamelize "ABC")))

(defun transform-by-function-map (function-map key value &optional (default-transform 'identity))
  (funcall (gethash key
                    function-map
                    default-transform)
           value))

(defun alist-to-initargs (alist value-map)
  (mapcan (destructuring-lambda ((key . value))
            (let* ((key (string-case:string-case (key :default key)
                          ("NotificationARNs" "NotificationArns")))
                   (initarg (make-keyword (string-upcase (decamelize key)))))
              (list initarg
                    (transform-by-function-map value-map
                                               initarg
                                               value))))
          alist))

(defun extract-stack (aws-result)
  (flet ((as-keyword (v) (alexandria:make-keyword (car v))))
    (apply #'make-instance 'stack
           (alist-to-initargs aws-result
                              (fw.lu:alist-string-hash-table
                               `((:outputs . extract-listq)
                                 (:capabilities . extract-list)
                                 (:creation-time . car)
                                 (:notification-arns . car)
                                 (:stack-id . car)
                                 (:stack-name . car)
                                 (:description . car)
                                 (:stack-status . ,#'as-keyword)
                                 (:disable-rollback . car)
                                 (:tags . extract-list)
                                 (:rollback-configuration . car)
                                 (:drift-information . car)
                                 (:enable-termination-protection . car)
                                 (:parameters . extract-list)))))))

(defun stack-for-name (name)
  (let* ((aws-result (car (extract-list (cdar (aws/cloudformation:describe-stacks :stack-name name)))))
         (the-stack (extract-stack aws-result)))
    the-stack))

(defun print-kvs (formatter stream data)
  (mapcar (destructuring-lambda (((_ k) (__ v)))
            (declare (ignore _ __))
            (funcall formatter stream k v))
          data))

(defmacro tagged-kv-formatter (tag)
  `(formatter ,(format nil "~a ~~a ~~a~~%" tag)))

(defun stack-outputs (the-stack)
  (print-kvs (tagged-kv-formatter "OUTPUT")
             (outputs the-stack)))

(defun stack-parameters (the-stack)
  (print-kvs (tagged-kv-formatter "PARAMETERS")
             (parameters the-stack)))

(defun stack-parameters-main (name)
  (stack-parameters (stack-for-name name)))

(defun stack-outputs-main (name)
  (stack-outputs (stack-for-name name)))

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

(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *cloud-watcher-synopsis*))
         (files (clon:remainder :context context)))
    (cond ((clon:getopt :long-name "help") (clon:help))
          ((clon:getopt :long-name "watch") (watch-stack (car files)))
          ((clon:getopt :long-name "outputs") (stack-outputs-main (car files)))
          ((clon:getopt :long-name "parameters") (stack-parameters-main (car files))))))

(defun dump ()
  "Create an executable with the command-line interface defined above."
  (clon:dump "cloud-watcher" main))

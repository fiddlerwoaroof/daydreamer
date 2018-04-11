(defpackage :cloud-watcher.aws-result
  (:use :cl :fw.lu :alexandria :st)
  (:export
   #:stack
   #:outputs
   #:reader
   #:creation-time
   #:notification-arns
   #:stack-id
   #:stack-name
   #:description
   #:stack-status
   #:disable-rollback
   #:tags
   #:deletion-time
   #:rollback-configuration
   #:drift-information
   #:enable-termination-protection
   #:parameters
   #:start-date-time
   #:end-date-time
   #:creation-date-time
   #:extract-timeline
   #:extract-list
   #:extract-stack
   #:timeline))
(in-package :cloud-watcher.aws-result)

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

(defun transform-by-function-map (function-map key value &optional (default-transform 'identity))
  (funcall (gethash key
                    function-map
                    default-transform)
           value))

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


(defmacro tagged-kv-formatter (tag)
  `(formatter ,(format nil "~a ~~a ~~a~~%" tag)))

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
   (%deletion-time :initarg :deletion-time :reader deletion-time)
   (%rollback-configuration :initarg :rollback-configuration :reader rollback-configuration)
   (%drift-information :initarg :drift-information :reader drift-information)
   (%enable-termination-protection :initarg :enable-termination-protection :reader enable-termination-protection)
   (%parameters :initarg :parameters :reader parameters)))

(defclass timeline ()
  ((%start-date-time :initarg :start-date-time :reader start-date-time)
   (%end-date-time :initarg :end-date-time :reader end-date-time)
   (%creation-date-time :initarg :creation-date-time :reader creation-date-time)))

(defun extract-timeline (aws-result)
  (labels ((cdr-assoc (key list) (cdr (assoc key list :test #'equal)))
           (get-in (keys alist)
             #+null
             (reduce (lambda (accum next)
                       ())
                     )
             (loop
                for key in keys
                for cur-alist = alist then accum
                for accum = (cdr-assoc key cur-alist)
                finally (return-from get-in accum))))
    (apply #'make-instance 'timeline
           (alist-to-initargs (get-in '("Status" "Timeline")
                                      aws-result)
                              (make-hash-table)))))

(defun extract-list (aws-result)
  (mapcar (destructuring-lambda ((list-item-marker . items))
            (if (string= list-item-marker "member")
                items
                (error 'invalid-result)))
          aws-result))

(defun extract-stack (aws-result)
  (flet ((as-keyword (v) (alexandria:make-keyword (car v))))
    (apply #'make-instance 'stack
           (alist-to-initargs aws-result
                              (fw.lu:alist-string-hash-table
                               `((:outputs . extract-list)
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


(defgeneric initialize-date (value)
  (:method ((value cons)) (local-time:parse-timestring (car value)))
  (:method (value) value))

(defmethod initialize-instance :after ((instance timeline) &key)
  (with-slots ((s-sdt %start-date-time)
               (s-edt %end-date-time)
               (s-cdt %creation-date-time)) instance
    (setf s-sdt (initialize-date s-sdt)
          s-edt (initialize-date s-edt)
          s-cdt (initialize-date s-cdt))))

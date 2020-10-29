(defpackage :daydreamer.aws-result
  (:use :cl :fw.lu :alexandria)
  (:export #:stack
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
           #:timeline
           #:tagged-kv-formatter
           #:extract-stack-resource
           #:stack-resource
           #:physical-resource-id
           #:resource-status
           #:logical-resource-id
           #:timestamp
           #:resource-type
           #:resource-status-reason))
(in-package :daydreamer.aws-result)

(defgeneric stack-name (stack)
  (:method ((stack string))
    stack))

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
  ((%outputs :initarg :outputs :reader outputs :initform (list))
   (%capabilities :initarg :capabilities :reader capabilities)
   (%last-updated-time :initarg :last-updated-time :reader last-updated-time)
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
   (%template-description :initarg :template-description :reader template-description)
   (%parameters :initarg :parameters :reader parameters :initform (list))))

(defmethod initialize-instance ((instance stack) &rest initargs &key &allow-other-keys)
  (let ((available-initargs (alexandria:mappend 'closer-mop:slot-definition-initargs
                                                (closer-mop:class-slots (find-class 'stack)))))
    (apply #'call-next-method
           instance
           (loop for (key value) on initargs by #'cddr
                 when (member key available-initargs)
                   nconc (list key value)))))

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

(defun extract-list (aws-result &optional (extractor 'identity))
  (mapcan (alexandria:compose (lambda (v)
                                (loop (restart-case (return (list (funcall extractor v)))
                                        (skip ()
                                          :report "Skip current item"
                                          (return ())))))
                              (destructuring-lambda ((list-item-marker . items))
                                (if (string= list-item-marker "member")
                                    items
                                    (error 'invalid-result))))
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
                                 (:template-description . car)
                                 (:parameters . extract-list)))))))

(defparameter *stack-statuses*
  '("CREATE_COMPLETE" "CREATE_IN_PROGRESS" "CREATE_FAILED"
    "DELETE_COMPLETE" "DELETE_FAILED" "DELETE_IN_PROGRESS"
    "REVIEW_IN_PROGRESS" "ROLLBACK_COMPLETE" "ROLLBACK_FAILED"
    "ROLLBACK_IN_PROGRESS" "UPDATE_COMPLETE"
    "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS" "UPDATE_IN_PROGRESS"
    "UPDATE_ROLLBACK_COMPLETE"
    "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
    "UPDATE_ROLLBACK_FAILED" "UPDATE_ROLLBACK_IN_PROGRESS"))

#+nil
(aws-sdk/services/cloudformation:list-stacks
 :stack-status-filter '("UPDATE_COMPLETE" "UPDATE_IN_PROGRESS"
                        "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS" "UPDATE_ROLLBACK_COMPLETE"
                        "UPDATE_ROLLBACK_IN_PROGRESS" "UPDATE_ROLLBACK_FAILED"
                        "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS" "CREATE_FAILED"
                        "CREATE_COMPLETE" "CREATE_IN_PROGRESS"))

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

(defclass stack-resource ()
  ((%physical-resource-id :initarg :physical-resource-id :reader physical-resource-id)
   (%resource-status :initarg :resource-status :reader resource-status)
   (%resource-status-reason :initarg :resource-status-reason :reader resource-status-reason :initform nil)
   (%stack-id :initarg :stack-id :reader stack-id)
   (%stack-name :initarg :stack-name :reader stack-name)
   (%drift-information :initarg :drift-information :reader drift-information)
   (%logical-resource-id :initarg :logical-resource-id :reader logical-resource-id)
   (%timestamp :initarg :timestamp :reader timestamp)
   (%resource-type :initarg :resource-type :reader resource-type)))

(defmethod print-object ((o stack-resource) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~a (~a): ~a"
            (logical-resource-id o)
            (resource-type o)
            (resource-status o))))

(defun extract-stack-resource (it)
  (apply 'make-instance 'daydreamer.aws-result::stack-resource
         (daydreamer.aws-result::alist-to-initargs
          it (fw.lu:alist-string-hash-table
              `((:physical-resource-id . car)
                (:resource-status . car)
                (:resource-status-reason . car)
                (:stack-id . car)
                (:stack-name . car)
                (:drift-information . car)
                (:logical-resource-id . car)
                (:timestamp . car)
                (:resource-type . car))))))

(defclass stack-template ()
  ((template-body :initarg :template-body :reader template-body)
   (stages-available :initarg :stages-available :reader stages-available)))


#+(or)
(deftest decamelize ()
  (should be equal "a" (decamelize "A"))
  (should be equal "a" (decamelize "a"))
  (should be equal "outputs" (decamelize "Outputs"))
  (should be equal "outputs-outputs" (decamelize "OutputsOutputs"))
  (should be equal "a-b-c" (decamelize "ABC")))

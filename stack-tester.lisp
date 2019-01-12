(defpackage :daydreamer.stack-tester
  (:use :cl)
  (:import-from :daydreamer.aws-result :extract-list :extract-stack :stack-name :stack-status)
  (:export ))
(in-package :daydreamer.stack-tester)

(defmacro with-api-response ((stream success &rest r &key indent) &body forms)
  (declare (ignore indent))
  `(yason:with-output (,stream ,@r)
     (yason:with-object ()
       (yason:encode-object-element "success" (if ,success 'yason:true 'yason:false))
       (yason:with-object-element ("result")
         ,@forms))))

(defun encode-stack (name stack-status)
  (yason:with-object ()
    (fw.lu:vector-destructuring-bind (verb status)
        (fwoar.string-utils:split #\_ (string-downcase stack-status)
                                  :count 2)
      (yason:encode-object-element "name" name)
      (yason:encode-object-element "verb" verb)
      (yason:encode-object-element "status" status))))

(defun encode-result (result)
  (yason:with-array ()
    (loop for (name value) in result
       do (encode-stack name value))))

(defun get-active-stack-statuses ()
  (let ((get-information (serapeum:juxt 'stack-name
                                        'stack-status)))
    (mapcar get-information
            (mapcar 'extract-stack
                    (extract-list
                     (cdar (aws-sdk/services/cloudformation:list-stacks 
                            :stack-status-filter '("UPDATE_COMPLETE" "UPDATE_IN_PROGRESS"
                                                   "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS" "UPDATE_ROLLBACK_COMPLETE"
                                                   "UPDATE_ROLLBACK_IN_PROGRESS" "UPDATE_ROLLBACK_FAILED" 
                                                   "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS" "CREATE_FAILED"
                                                   "CREATE_COMPLETE" "CREATE_IN_PROGRESS"
                                                   "DELETE_IN_PROGRESS" "DELETE_FAILED"))))))))
(defclass status-server (hunchentoot:acceptor)
  ())

(defmethod hunchentoot:acceptor-dispatch-request ((server status-server) request)
  (setf (hunchentoot:content-type*) "application/json")
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (string-case:string-case ((hunchentoot:script-name request))
    ("/healthchecks" (with-output-to-string (s)
                       (with-api-response (s t :indent t)
                         (yason:with-object ()
                           (yason:encode-object-element "ElasticSearch Cluster Status" (string-downcase (get-es-cluster-status (local-time:now))))
                           (yason:encode-object-element "ElasticSearch Backup Status" (string-downcase (get-es-backup-status (local-time:now))))))))
    ("/stacks" (with-output-to-string (s)
                 (with-api-response (s t :indent t)
                   (encode-result (get-active-stack-statuses)))))))

(defun aws-host (service region)
  (format nil "~(~A~).~(~A~).amazonaws.com" service region))

(defun aws-request (&key (path "/") service method params headers payload (session aws-sdk/api:*session*))
  (let ((credentials (or (aws-sdk/session:session-credentials session)
                         (aws-sdk/credentials:aws-credentials)))
        (region (aws-sdk/session:session-region session)))

    (unless credentials
      (error "No credentials are found"))
    (unless region
      (error "AWS region is not configured"))

    (let ((host (aws-host service region))
          (aws-sign4:*aws-credentials* (or aws-sign4:*aws-credentials*
                                           (lambda () (aws-sdk/credentials:credential-keys credentials)))))
      (multiple-value-bind (authorization x-amz-date)
          (aws-sign4:aws-sign4 :region region
                               :service service
                               :method method
                               :host host
                               :path path
                               :params params
                               :headers headers
                               :payload (or payload ""))
        (dex:request (format nil "https://~A~A?~A" host path
                             (quri:url-encode-params params))
                     :method method
                     :keep-alive nil
                     :headers `(("Authorization" . ,authorization)
                                ("X-Amz-Date" . ,x-amz-date)
                                ,@(aws-sdk/credentials:credential-headers credentials)
                                ("Content-Type" . "application/json")
                                ("Accept" . "application/json")
                                ,@headers)
                     :content payload)))))

#+null
(get-metric-statistics "es-pseudo-prod.insights.cj.com" "ClusterStatus.green" '(("ClusterHealth" . "ClusterStatus"))
                       (local-time:parse-timestring "2018-06-11T07:00:00.000Z")
                       (local-time:parse-timestring "2018-06-11T07:05:00.000Z") 300 '("Maximum")
  )

(defun get-metric-statistics (namespace metric-name dimensions start-time end-time period statistics)
  (aws-request :service "monitoring" 
               :method :post 
               :params `(("Action" . "GetMetricStatistics")
                         ("Version" . "2010-08-01")
                         ("Namespace" . ,namespace)
                         ("MetricName" . ,metric-name)
                         ,@(loop for (dimension-name . dimension-value) in dimensions
                              for idx from 1
                              append (list (cons (format nil "Dimensions.member.~d.Name" idx)
                                                 dimension-name)
                                           (cons (format nil "Dimensions.member.~d.Value" idx)
                                                 dimension-value)))
                         ("StartTime" . ,(local-time:format-timestring nil start-time
                                                                       :format '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\T (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2) #\. (:USEC 3) :GMT-OFFSET-OR-Z)
                                                                       :timezone local-time:+utc-zone+))
                         ("EndTime" . ,(local-time:format-timestring nil end-time
                                                                     :format '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\T (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2) #\. (:USEC 3) :GMT-OFFSET-OR-Z)
                                                                     :timezone local-time:+utc-zone+))
                         ("Period" . ,period)
                         ,@(loop for statistic in statistics
                              for idx from 1
                              collect (cons (format nil "Statistics.member.~d" idx) statistic)))))

(defun get-metric-statistics-for-time (namespace metric-name dimensions start-time statistics)
  (yason:parse (get-metric-statistics namespace metric-name dimensions start-time (local-time:timestamp+ start-time 5 :minute) 300 statistics)))

(defun extract-datapoints (response statistics)
  (funcall (apply 'serapeum:juxt (mapcar 'data-lens:key statistics))
           (car (fw.lu:pick '("GetMetricStatisticsResponse" "GetMetricStatisticsResult" "Datapoints")
                            response))))

(defun get-es-cluster-status (start-time)
  (flet ((get-healthcheck (metric-name)
           (let ((statistics '("Maximum")))
             (extract-datapoints
              (get-metric-statistics-for-time "es.pseudo-prod.insights.cj.com"
                                              metric-name
                                              '(("ClusterHealth" . "ClusterStatus"))
                                              (local-time:timestamp- start-time 5 :minute)
                                              statistics)
              statistics))))
    (uiop:nest
     (destructuring-bind (green) (get-healthcheck "ClusterStatus.green"))
     (destructuring-bind (yellow) (get-healthcheck "ClusterStatus.yellow"))
     (destructuring-bind (red) (get-healthcheck "ClusterStatus.red"))
     (cond ((and (numberp red) (> red 0)) :red)
           ((and (numberp yellow) (> yellow 0)) :yellow)
           ((and (numberp green) (> green 0)) :green)
           (t :invalid)))))

(defun get-es-backup-status (start-time)
  (flet ((get-healthcheck (metric-name)
           (let ((statistics '("Maximum")))
             (extract-datapoints
              (yason:parse
               (get-metric-statistics "es-backup.pseudo-prod.insights.cj.com"
                                      metric-name
                                      '(("ESBackup" . "ESBackupStatus"))
                                      (local-time:timestamp-minimize-part start-time :hour)
                                      (local-time:timestamp+ (local-time:timestamp-minimize-part start-time :hour)
                                                             1 :day)
                                      #.(* 24 60 60)
                                      statistics))
              statistics))))
    (uiop:nest
     (destructuring-bind (green) (get-healthcheck "ESBackupStatus.GREEN"))
     (destructuring-bind (yellow) (get-healthcheck "ESBackupStatus.YELLOW"))
     (destructuring-bind (red) (get-healthcheck "ESBackupStatus.RED"))
     (cond ((and (numberp red) (> red 0))  :red)
           ((and (numberp yellow) (> yellow 0)) :yellow)
           ((and (numberp green) (> green 0)) :green)
           (t :invalid)))))

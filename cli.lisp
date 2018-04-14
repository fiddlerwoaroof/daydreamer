(defpackage :cloud-watcher.cli
  (:import-from :cloud-watcher.main :stack-parameters :stack-outputs :stack-for-name :stack-info)
  (:import-from :cloud-watcher.aws-result :start-date-time :end-date-time)
  (:import-from :serapeum :op)
  (:import-from :clon :defsynopsis :group :flag :stropt)
  (:use :cl :st)
  (:export options
           #:*cloud-watcher-synopsis*
           #:dump))

(in-package :cloud-watcher.cli)

(defparameter *cloud-watcher-synopsis*
  (defsynopsis (:postfix "ARGS...")
    (group (:header "actions")
           (flag :short-name "p" :long-name "parameters" :description "show stack parameters")
           (flag :short-name "o" :long-name "outputs" :description "show stack outputs")
           (flag :short-name "w" :long-name "watch" :description "watch a cloudformation stack until it's done processing")
           (flag :short-name "i" :long-name "info" :description "get parameters, status and output of a stack")
           #+null
           (flag :short-name "s" :long-name "start")
           (stropt :long-name "aws-region" :default-value "us-west-2")
           (flag :short-name "u" :long-name "update"))
    (group (:header "misc")
           (flag :long-name "rebuild")
           (flag :long-name "self-test")
           (flag :long-name "help"))))

(defun stack-parameters-main (name)
  (stack-parameters (stack-for-name name)))

(defun stack-outputs-main (name)
  (stack-outputs (stack-for-name name)))

(defun stack-info-main (name)
  (stack-info (stack-for-name name)))

(defun run-tests ()
  (st:test :package (find-package :cloud-watcher.aws-result))
  (st:test :package (find-package :cloud-watcher.main))
  (st:test :package (find-package :cloud-watcher.cli)))

(defun dump ()
  "Create an executable with the command-line interface defined above."
  (handler-bind ((sb-ext:name-conflict (lambda (c)
                                         (declare (ignore c))
                                         (invoke-restart-interactively 'sb-ext:resolve-conflict))))
    (let ((sb-ext:*on-package-variance* '(:warn (:cloud-watcher.aws-result
                                                 :cloud-watcher.main
                                                 :cloud-watcher.cli)
                                          :error t)))
      (asdf:load-system :cloud-watcher :force t)))
  (clon:dump "cloud-watcher" main))

(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *cloud-watcher-synopsis*))
         (files (clon:remainder :context context))
         (region (clon:getopt :long-name "aws-region"))
         (aws-sdk/api:*session* (aws-sdk/session:make-session :region region)))

    (format *error-output* "~&IN REGION: ~a~%" region)

    (cond ((clon:getopt :long-name "help") (clon:help))
          ((clon:getopt :long-name "info") (stack-info-main (car files)))
          ((clon:getopt :long-name "watch") (cloud-watcher.main:watch-stack (car files)))
          ((clon:getopt :long-name "outputs") (stack-outputs-main (car files)))
          ((clon:getopt :long-name "parameters") (stack-parameters-main (car files)))
          ((clon:getopt :long-name "self-test") (run-tests))
          ((clon:getopt :long-name "rebuild") (dump)))))

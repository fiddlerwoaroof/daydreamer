(eval-when (:execute :load-toplevel :compile-toplevel)
  (net.didierverna.clon:nickname-package))

(defpackage :daydreamer.cli
  (:import-from :daydreamer.main :stack-parameters :stack-outputs :stack-for-name :stack-info)
  (:import-from :daydreamer.aws-result :start-date-time :end-date-time)
  (:import-from :serapeum :op)
  (:import-from :clon :defsynopsis :group :flag :stropt)
  (:use :cl :st)
  (:export options
           #:*daydreamer-synopsis*
           #:dump))

(in-package :daydreamer.cli)

(defparameter *daydreamer-synopsis*
  (defsynopsis (:postfix "ARGS...")
    (group (:header "actions")
      (flag :short-name "s" :long-name "stacks" :description "show stack information")
      (flag :short-name "p" :long-name "parameters" :description "show stack parameters")
      (flag :short-name "o" :long-name "outputs" :description "show stack outputs")
      (flag :short-name "r" :long-name "resources" :description "show stack resources")
      (flag :short-name "w" :long-name "watch" :description "watch a cloudformation stack until it's done processing")
      (flag :short-name "i" :long-name "info" :description "get parameters, status and output of a stack")
      #+null
      (flag :short-name "s" :long-name "start")
      (stropt :long-name "aws-region"
              :description "explicitly pick AWS Region"
              :env-var "AWS_DEFAULT_REGION")
      (stropt :long-name "aws-profile"
              :description "aws profile to use for authentication"
              :env-var "AWS_PROFILE")
      (stropt :long-name "aws-access-key-id"
              :description "aws access-key-id to use for authentication"
              :env-var "AWS_ACCESS_KEY_ID")
      (stropt :long-name "aws-secret-access-key"
              :description "aws secret-access-key to use for authentication"
              :env-var "AWS_SECRET_ACCESS_KEY")
      (stropt :long-name "aws-session-token"
              :description "aws session-token to use for authentication"
              :env-var "AWS_SESSION_TOKEN")
      #+null
      (flag :short-name "u" :long-name "update"))
    (group (:header "misc")
      (flag :long-name "rebuild")
      (flag :long-name "self-test")
      (flag :long-name "help"))))

(defun stacks-main ()
  (mapcar (lambda (s)
            (format t "~3&STACK ~a ~a~2%"
                    (daydreamer.aws-result:stack-name s)
                    (daydreamer.aws-result:stack-status s))
            (stack-info s t t t))
          (mapcar 'daydreamer.aws-result:extract-stack
                  (daydreamer.aws-result:extract-list
                   (cdar
                    (aws/cloudformation:describe-stacks))))))

(defun stack-parameters-main (name)
  (stack-parameters (stack-for-name name)))

(defun stack-outputs-main (name)
  (stack-outputs (stack-for-name name)))

(defun stack-info-main (name status parameters outputs resources)
  (stack-info (stack-for-name name) status parameters outputs resources))

(defun run-tests ()
  (st:test :package (find-package :daydreamer.aws-result))
  (st:test :package (find-package :daydreamer.main))
  (st:test :package (find-package :daydreamer.cli)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun dump ()
    "Create an executable with the command-line interface defined above."
    (handler-bind ((sb-ext:name-conflict (lambda (c)
                                           (declare (ignore c))
                                           (invoke-restart-interactively 'sb-ext:resolve-conflict))))
      (let ((sb-ext:*on-package-variance* '(:warn t #+nil(:daydreamer.aws-result
                                                           :daydreamer.main
                                                           :daydreamer.cli)
                                                  #+nil(:error t))))
        (asdf:load-system :daydreamer :force t)))
    (clon:dump "daydreamer" main)))

(defun main ()
  (cl+ssl:reload)
  (let* ((context (net.didierverna.clon:make-context :synopsis *daydreamer-synopsis*))
         (files (clon:remainder :context context))
         (region (clon:getopt :long-name "aws-region"))
         (profile (clon:getopt :long-name "aws-profile"))
         (access-key-id (clon:getopt :long-name "aws-access-key-id"))
         (secret-access-key (clon:getopt :long-name "aws-secret-access-key"))
         (session-token (clon:getopt :long-name "aws-session-token")))

    (cond (access-key-id
           (setf aws:*session*
                 (aws:make-session
                  :credentials (if session-token
                                   (aws:make-credentials
                                    :access-key-id access-key-id
                                    :secret-access-key secret-access-key
                                    :session-token session-token
                                    )
                                   (aws:make-credentials
                                    :access-key-id access-key-id
                                    :secret-access-key secret-access-key
                                    :session-token nil
                                    ))
                  :region region)))
          ((and region profile)
           (aws:init-session
            :region region
            :profile profile))
          (region
           (aws:init-session
            :region region))
          (profile
           (aws:init-session
            :profile profile))
          (t
           (aws:init-session)))

    (format *error-output* "~&IN REGION: ~a~%WITH PROFILE: ~a~%"
            (aws:session-region aws:*session*)
            profile)

    (cond ((clon:getopt :long-name "help") (clon:help))
          ((clon:getopt :long-name "watch") (daydreamer.main:watch-stack (car files)))
          ((clon:getopt :long-name "stacks") (stacks-main))
          ((clon:getopt :long-name "self-test") (run-tests))
          ((clon:getopt :long-name "rebuild")
           (load (compile-file (load-time-value
                                (asdf:system-relative-pathname :daydreamer "cli.lisp"))))
           (dump))
          (t
           (let ((info (clon:getopt :long-name "info"))
                 (parameters (clon:getopt :long-name "parameters"))
                 (outputs (clon:getopt :long-name "outputs"))
                 (resources (clon:getopt :long-name "resources")))
             (stack-info-main (car files)
                              info
                              (or info parameters)
                              (or info outputs)
                              resources))))))

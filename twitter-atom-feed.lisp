;;;; twitter-atom-feed.lisp

(in-package #:twitter-atom-feed)

(defun prompt-string (prompt)
  "Prompt user in STDIN for a string."
  (format t "~a: " prompt)
  (read-line))

(defun prompt-to-authenticate ()
  "Prompt user to authenticate Twitter API credentials."
  (format t "Initializing credentials…~%")
  (let ((key (prompt-string "Twitter API key"))
        (secret (prompt-string "Twitter API secret")))
    (format t "~a~%" (chirp:initiate-authentication :method :pin :api-key key :api-secret secret))
    (chirp:complete-authentication (prompt-string "PIN from URL"))
    (twitter-atom-feed.files:write-data
     :api-key chirp:*oauth-api-key*
     :api-secret chirp:*oauth-api-secret*
     :access-token chirp:*oauth-access-token*
     :access-secret chirp:*oauth-access-secret*)))

(defun authenticate ()
  "Authenticate Twitter API credentials."
  (let ((plst (twitter-atom-feed.files:read-data)))
    (when (null plst)
      (prompt-to-authenticate)
      (setf plst (twitter-atom-feed.files:read-data)))
    (setf chirp:*oauth-api-key* (getf plst :api-key)
          chirp:*oauth-api-secret* (getf plst :api-secret)
          chirp:*oauth-access-token* (getf plst :access-token)
          chirp:*oauth-access-secret* (getf plst :access-secret))
    (chirp:account/verify-credentials)))

(opts:define-opts
  (:name :help
   :description "help"
   :short #\h
   :long "help")
  (:name :bind
   :description "Atom feed address"
   :short #\b
   :long "bind"
   :arg-parser #'string
   :meta-var "ADDRESS"
   :default "localhost")
  (:name :port
   :description "Atom feed port"
   :short #\p
   :long "port"
   :arg-parser #'parse-integer
   :meta-var "PORT"
   :default 8080))

(defun opts-describe-and-exit (&optional (exit-code 0))
  "Show command usage and exit with EXIT-CODE."
  (opts:describe
   :usage-of "twitter-atom-feed")
  (opts:exit exit-code))

(defun command-line ()
  "Command line entry point for twitter-atom-feed."
  (multiple-value-bind (opts _free-args)
      (handler-case (opts:get-opts)
        (opts:unknown-option (condition)
          (format t "fatal: unknown option ~s~%"
                  (opts:option condition))
          (opts-describe-and-exit 2))
        (opts:arg-parser-failed (condition)
          (format t "fatal: cannot parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition))
          (opts:exit 2)))
    (when (getf opts :help)
      (opts-describe-and-exit))
    (handler-case (bt:join-thread
                   (hunchentoot::acceptor-process
                    (hunchentoot::acceptor-taskmaster
                     (start (getf opts :bind) (getf opts :port)))))
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl  ccl:interrupt-signal-condition
       #+clisp system::simple-interrupt-condition
       #+ecl ext:interactive-interrupt
       #+allegro excl:interrupt-signal ()
        (opts:exit 0))
      (error (err)
        (format t "something happened: ~&~s~&" err)
        (opts:exit 1)))))

(defun start (&optional (address "localhost") (port 8080))
  "Start atom feed server."
  (authenticate)
  (twitter-atom-feed.server:start address port))

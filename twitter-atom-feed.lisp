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
    (write-data
     :api-key chirp:*oauth-api-key*
     :api-secret chirp:*oauth-api-secret*
     :access-token chirp:*oauth-access-token*
     :access-secret chirp:*oauth-access-secret*)))

(defun authenticate ()
  "Authenticate Twitter API credentials."
  (let ((plst (read-data)))
    (when (null plst)
      (prompt-to-authenticate)
      (setf plst (read-data)))
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
   :default 8080)
  (:name :opml
   :description "Print friends list in OPML format"
   :long "opml"))

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
    (cond ((getf opts :help) (opts-describe-and-exit))
          ((getf opts :opml)
           (export-opml
            :address (getf opts :bind)
            :port (getf opts :port))
           (opts:exit 0))
          (t (handler-case (bt:join-thread
                            (hunchentoot::acceptor-process
                             (hunchentoot::acceptor-taskmaster
                              (start (getf opts :bind) (getf opts :port)))))
               (#+sbcl sb-sys:interactive-interrupt
                #+ccl ccl:interrupt-signal-condition
                #+clisp system::simple-interrupt-condition
                #+ecl ext:interactive-interrupt
                #+allegro excl:interrupt-signal ()
                 (opts:exit 0))
               (error (err)
                 (format t "something happened: ~&~s~&" err)
                 (opts:exit 1)))))))

(defun start (&optional (address "localhost") (port 8080) async)
  "Start atom feed server."
  (load-config)
  (authenticate)
  (start-server address port async))

(defun export-opml (&key (address "localhost") (port 8080) (stream *standard-output*))
  "Export friends (accounts the user follows) list to stream"
  (let ((user (authenticate)))
    (who:with-html-output (stream nil :indent t :prologue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
      ((:opml :version "1.0")
       (:head (:title "Twitter Friends List"))
       (:body
        ((:outline :title "Twitter Friends List"
                   :text "Twitter Friends List")
         (loop :for friend :in (chirp:friends/list :user-id (chirp:id user)) :do
           (who:htm
            (:outline
             :type "rss"
             :version "Atom"
             :|xmlUrl| (format nil "http://~a:~a/user/id/~a" address port (chirp:id friend))
             :|htmlUrl| (format nil "https://twitter.com/~a" (chirp:screen-name friend))
             :title (chirp:name friend)
             :text (chirp:name friend))))))))
    (fresh-line stream)))

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

(defparameter *ui*
  (adopt:make-interface
   :name "twitter-atom-feed"
   :summary "Twitter Timeline → Atom Feed"
   :usage "[-h|--help] [--opml] [-p|--port PORT] [-b|--bind ADDRESS]"
   :help "Turn Twitter timelines into Atom feeds, hosted on http://ADDRESS:PORT"
   :contents
   (list (adopt:make-option 'help
          :long "help"
          :short #\h
          :help "Display help information and exit."
          :reduce (constantly t))
         (adopt:make-option 'opml
          :long "opml"
          :help "Print friends list in OPML format and exit."
          :reduce (constantly t))
         (adopt:make-option 'bind
          :parameter "ADDRESS"
          :long "bind"
          :short #\b
          :help "Specify alternate bind address (default: \"localhost\")"
          :initial-value "localhost"
          :reduce #'adopt:last)
         (adopt:make-option 'port
          :parameter "PORT"
          :long "port"
          :short #\p
          :help "Specify alternate port (default: 8080)"
          :initial-value 8080
          :reduce #'adopt:last
          :key #'parse-integer))))

(defun exit-on-signal (signo)
  (format *error-output* "~&received ~A~%" (trivial-signal:signal-name signo))
  (uiop:quit 1))

(defun command-line ()
  "Command line entry point for twitter-atom-feed."
  (setf (trivial-signal:signal-handler :quit) #'exit-on-signal)
  (multiple-value-bind (_free-args opts)
      (handler-case (adopt:parse-options *ui*)
        (error (c) (adopt:print-error-and-exit c)))
    (cond ((gethash 'help opts) (adopt:print-help-and-exit *ui*))
          ((gethash 'opml opts)
           (export-opml
            :address (gethash 'bind opts)
            :port (gethash 'port opts))
           (uiop:quit 0))
          (t (trivial-signal:signal-handler-bind
                 ((3 #'exit-on-signal))
               (handler-case (bt:join-thread
                              (hunchentoot::acceptor-process
                               (hunchentoot::acceptor-taskmaster
                                (start (gethash 'bind opts) (gethash 'port opts)))))
                 (usocket:unknown-error (_)
                   (uiop:quit 0))
                 (error (err)
                   (format t "something happened: ~&~s~&" err)
                   (uiop:quit 1))))))))

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
         (dolist (friend (chirp:friends/list :user-id (chirp:id user)))
           (who:htm
            (:outline
             :type "rss"
             :version "Atom"
             :|xmlUrl| (format nil "http://~a:~a/user/id/~a" address port (chirp:id friend))
             :|htmlUrl| (format nil "https://twitter.com/~a" (chirp:screen-name friend))
             :title (chirp:name friend)
             :text (chirp:name friend))))))))
    (fresh-line stream)))

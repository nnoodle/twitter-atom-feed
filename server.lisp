(in-package #:twitter-atom-feed.server)

;; github uses tag:github.com,2008
(defparameter +urn+ "urn:tag:twitter-atom-feed,2020"
  "Universal Resource Name for Atom feed.")

(defparameter +default-count+ 200
  "Default number of tweets to fetch.")

(defvar *acceptor* nil
  "Hunchentoot acceptor.")

(eval-when (:compile-toplevel :load-toplevel :execute) ; doesn't work without this part
  (pushnew :outline who:*html-empty-tags*)
  (setf who:*escape-char-p* (lambda (char) (find char "<>&'\""))
        who:*attribute-quote-char* #\"
        who:*html-no-indent-tags* (append '(:id :title :updated :name :uri) cl-who:*html-no-indent-tags*)))

(defun trunc-tweet (tweet to-length)
  "Truncate text TWEET to length of TO-LENGTH or less."
  (let ((text (cl-ppcre:regex-replace " +https://t\.co/[A-Za-z0-9]+$" tweet "")))
    (if (> (length text) to-length)
        (concatenate 'string (subseq text 0 to-length) "…")
        text)))

(defun real-tweet (tweet)
  "The retweeded tweet if it is a retweet."
  (if (chirp:retweet-p tweet)
      (chirp:retweeted-status tweet)
      tweet))

(defun write-atom-feed (tweets &key (stream *standard-output*) user)
  "Write an atom feed of all tweets to output."
  (let ((feed-id (format nil "~a:~a" +urn+ (if user (chirp:id user) "home")))
        (title (if user (format nil "~a" (chirp:name user)) "Twitter Home Feed"))
        (author-name (if user (format nil "@~a" (chirp:screen-name user)) "Twitter Feed"))
        (author-uri (if user (format nil "https://twitter.com/~a" (chirp:screen-name user)) "https://twitter.com/home")))
    (who:with-html-output (stream nil :indent t :prologue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
      ((:feed :xmlns "http://www.w3.org/2005/Atom")
       (:title (who:esc title))
       ;; (:link :rel "self" :type "application/atom+xml" :href "http://localhost:8080")
       (:updated (who:str (chirp:created-at (car tweets))))
       (:id (who:str feed-id))
       (:author
        (:name (who:esc author-name))
        (:uri (who:esc author-uri)))
       (loop :for tweet :in (mapcar #'real-tweet tweets) :do
         (who:htm
          (:entry
           (:id (who:fmt "~a:~a" +urn+ (chirp:id tweet)))
           (:title (who:esc
                    (who:conc (trunc-tweet (chirp:full-text tweet) 49)
                              (when (chirp:possibly-sensitive tweet) " 🔞"))))
           (:updated (who:str (chirp:created-at tweet)))
           (:link :rel "alternate" :type "text/html"
                  :href (format nil "https://twitter.com/~a/status/~a"
                                (chirp:screen-name (chirp:user tweet))
                                (chirp:id tweet)))
           (:author
            (:name (who:fmt "@~a" (chirp:screen-name (chirp:user tweet))))
            (:uri (who:fmt "https://twitter.com/~a" (chirp:screen-name (chirp:user tweet)))))
           ((:content :type "html")
            (who:str "<![CDATA[")
            (:p (who:str (chirp:text-with-markup tweet :text (chirp:full-text tweet))))
            (when (chirp::extended-entities tweet)
              (who:htm
               (:p (loop :for img :in (cdar (chirp::extended-entities tweet))
                         :for photo :from 1 :to (length (cdar (chirp::extended-entities tweet)))
                         :do (who:htm
                              (:a :class "status-media"
                                  :href (chirp:media-url-https img)
                                  :title (format nil "https://twitter.com/~a/status/~a/photo/~a"
                                                 (chirp:screen-name (chirp:user tweet))
                                                 (chirp:id tweet)
                                                 photo)
                                  (:img :src (chirp:media-url-https img))))))))
            (:p (who:str (chirp:source tweet)))
            (who:str "]]>"))))))
      nil)))

(defun get-many-tweets (timeline-fn &rest args)
  "Get more than 200 tweets from the timeline APIs."
  (let ((init-count (getf args :count))
        (countless-args (alexandria:remove-from-plist args :count)))
    (if (or (null init-count) (<= init-count 200))
        (apply timeline-fn args)
        (loop :for count :from (- init-count 200) :downto 1 :by 200
              :with init := (apply timeline-fn :count 200 countless-args)
              :with cur := init
              :if (null cur)
                :do (loop-finish)
              :else
                :append
                (setf cur (apply timeline-fn
                                 :count (min 200 count)
                                 :max-id (1- (chirp:id (car (last cur))))
                                 countless-args))
                :into acc
              :finally (return (nconc init acc))))))

(defmacro get-tweets (tweet-form &optional filters)
  "Wrap around tweet-form to get more than 200 tweets, filtered by FILTERS"
  (let ((tweets `(get-many-tweets #',(first tweet-form) ,@(rest tweet-form))))
    (if (null filters)
        tweets
        (let ((v (gensym))
              (f (gensym)))
          `(remove-if-not
            (lambda (,v)
              (loop :for ,f :in ,filters
                    :unless (funcall ,f ,v)
                      :do (return nil)
                    :finally (return t)))
            ,tweets)))))

(defun write-atom-feed-string (tweets user)
  "Wrap tweet-form around atom string writer."
  (with-output-to-string (stream)
    (write-atom-feed
     tweets
     :stream stream
     :user user)))

(defun @oauth-err (next)
  (handler-case (funcall next)
    (chirp:oauth-request-error (err)
      (setf (hunchentoot:return-code*) (chirp:http-status err))
      (setf (hunchentoot:content-type*) "text/plain")
      (format nil "~s~%" (chirp:http-body err)))))

(defun @atom (next)
  (setf (hunchentoot:content-type*) "application/atom+xml")
  (funcall next))

(easy-routes:defroute home-route
    ("/home" :decorators (@oauth-err @atom))
    (imagep (count :init-form +default-count+ :parameter-type 'integer))
  (write-atom-feed-string
   (get-tweets (chirp:statuses/home-timeline
                :count count :tweet-mode "extended")
               (if imagep (list #'chirp:extended-entities) nil))
   nil))

(easy-routes:defroute user-id-route
    ("/user/id/:id" :decorators (@oauth-err @atom))
    (imagep (count :init-form +default-count+ :parameter-type 'integer) &path (id 'integer))
  (write-atom-feed-string
   (get-tweets (chirp:statuses/user-timeline
                :user-id id :count count :tweet-mode "extended")
               (if imagep (list #'chirp:extended-entities) nil))
   (chirp:users/show :user-id id)))

(easy-routes:defroute user-name-route
    ("/user/name/:screen-name" :decorators (@oauth-err @atom))
    (imagep (count :init-form +default-count+ :parameter-type 'integer))
  (write-atom-feed-string
   (get-tweets (chirp:statuses/user-timeline
                :screen-name screen-name :count count :tweet-mode "extended")
               (if imagep (list #'chirp:extended-entities) nil))
   (chirp:users/show :screen-name screen-name)))

(easy-routes:defroute favicon-route ("/favicon.ico") ()
  (hunchentoot:redirect "https://abs.twimg.com/favicons/twitter.ico"))

(defun start (address port)
  "Starts Atom feed web server at http://ADDRESS:PORT"
  (setf *acceptor* (hunchentoot:start
                    (make-instance
                     'easy-routes:routes-acceptor
                     :address address
                     :port port
                     ;; simultaneous Twitter API calls will fail about half the time.
                     :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)))))

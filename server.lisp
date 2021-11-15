;;;; server.lisp

(in-package #:twitter-atom-feed)

(defparameter +urn+ "urn:tag:twitter-atom-feed,2020" ; github uses tag:github.com,2008
  "Universal Resource Name for Atom feed.")

(defparameter +default-count+ 200
  "Default number of tweets to fetch.")

(defvar *acceptor* nil
  "Hunchentoot acceptor.")

(eval-when (:compile-toplevel :load-toplevel :execute)
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

(defun pluck-video (video-info)
  "Pluck the video with the highest bitrate"
  (first (sort (cdr (assoc :variants video-info)) #'>
               :key (lambda (v) (or (cdr (assoc :bitrate v)) 0)))))

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
            (when (chirp:extended-entities tweet)
              (who:htm
               (:p (loop :for img :in (cdar (chirp:extended-entities tweet))
                         ;; expanded-url is wrong
                         :for photo :from 1 :to (length (cdar (chirp:extended-entities tweet)))
                         :do (if (chirp::video-info img)
                                 (let ((who:*empty-attribute-syntax* t)
                                       (video (pluck-video (chirp::video-info img))))
                                   (who:htm ((:video :controls t :loop t :playsinline t
                                                     :poster (chirp:media-url-https img))
                                             (:source :src (cdr (assoc :url video))
                                                      :type (cdr (assoc :content-type video))))
                                            (:p ((:a :href (cdr (assoc :url video)))
                                                 "Direct link to video."))))
                                 (who:htm
                                  (:a :class "status-media"
                                      :href (chirp:media-url-https img)
                                      :title (format nil "https://twitter.com/~a/status/~a/photo/~a"
                                                     (chirp:screen-name (chirp:user tweet))
                                                     (chirp:id tweet)
                                                     photo)
                                      (:img :src (chirp:media-url-https img)))))))))
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

(def-memoized-function (compose-filters :test #'equalp) (filters)
  "Compose together filters from the twitter-atom-feed-filters package into one function."
  (let* ((filters (mapcar
                   (lambda (s) (string-trim " " s))
                   (delete-duplicates (split-sequence #\, filters :remove-empty-subseqs t)
                                      :test #'string-equal)))
         (symbols (loop :for symbol :being :the :present-symbols
                          :in '#:twitter-atom-feed-filters
                        :collect symbol))
         (filters (reduce
                   (lambda (col symbol)
                     (cond ((find (symbol-name symbol) filters :test #'string-equal)
                            (cons (symbol-function symbol) col))
                           ((find (concatenate 'string "-" (symbol-name symbol)) filters :test #'string-equal)
                            (cons (complement (symbol-function symbol)) col))
                           (t col)))
                   (delete-duplicates symbols)
                   :initial-value nil)))
    (lambda (tweet)
      (loop :for f :in filters
            :unless (handler-case (funcall f tweet)
                      (error (e)
                        (format *error-output* "ERROR CALLING ~S ON TWEET ~S: ~A" f (chirp:id tweet) e)
                        nil))
              :do (return nil)
            :finally (return t)))))

(defun @oauth-err (next)
  (handler-case (funcall next)
    (chirp:oauth-request-error (err)
      (setf (hunchentoot:return-code*) (chirp:http-status err))
      (setf (hunchentoot:content-type*) "text/plain")
      (format nil "~s~%" (chirp:http-body err)))))

(defun @eof-error (next)
  (handler-case (funcall next)
    (end-of-file (err)
      (setf (hunchentoot:return-code*) 500)
      (setf (hunchentoot:content-type*) "text/plain")
      (format t "EOF ERROR ~s~%" err)
      (format nil "~s~%" err))))

(defun @atom (next)
  (setf (hunchentoot:content-type*) "application/atom+xml")
  (funcall next))

(defmacro define-tweet-route (name path tweet-form &key user-form parameters)
  "A very leaky abstraction."
  `(easy-routes:defroute ,name
       (,path :decorators (@oauth-err @eof-error @atom))
       (filters (count :init-form +default-count+ :parameter-type 'integer) ,@parameters)
     (with-output-to-string (stream)
       (write-atom-feed
        (setf (hunchentoot:aux-request-value :tweets)
              (remove-if-not (compose-filters filters)
                             (get-many-tweets #',(first tweet-form)
                                              :count count
                                              :tweet-mode "extended"
                                              ,@(rest tweet-form))))
        :stream stream
        :user ,user-form))))

(define-tweet-route home-route "/home" (chirp:statuses/home-timeline))
(define-tweet-route user-id-route "/user/id/:id"
  (chirp:statuses/user-timeline :user-id id)
  :parameters (&path (id 'integer))
  :user-form (chirp:users/show :user-id id))
(define-tweet-route user-name-route "/user/name/:screen-name"
  (chirp:statuses/user-timeline :screen-name screen-name)
  :user-form (chirp:users/show :screen-name screen-name))

(easy-routes:defroute favicon-route ("/favicon.ico") ()
  (hunchentoot:redirect "https://abs.twimg.com/favicons/twitter.ico"))

(defmethod hunchentoot:acceptor-log-access ((acceptor hunchentoot:acceptor) &key return-code)
  (hunchentoot::with-log-stream (stream (hunchentoot:acceptor-access-log-destination acceptor)
                                        hunchentoot::*access-log-lock*)
    (format stream "[~A]~@[ ~3D Tweets~] ~A~@[?~A~]~%"
            (hunchentoot::iso-time)
            (and (hunchentoot:aux-request-value :tweets)
                 (length (hunchentoot:aux-request-value :tweets)))
            (hunchentoot:script-name*)
            (hunchentoot:query-string*))))

(defun start-server (address port async)
  "Starts Atom feed web server at http://ADDRESS:PORT"
  (when *memoize-tweets-p*
    (memoize-function 'get-many-tweets
                      :key (lambda (lst)
                             (let ((args (cdr lst)))
                               (or (getf args :screen-name)
                                   (getf args :user-id))))
                      :test #'equal))
  (setf *acceptor* (hunchentoot:start
                    (make-instance
                     'easy-routes:routes-acceptor
                     :address address
                     :port port
                     ;; simultaneous Twitter API calls will fail about half the time.
                     ;; but async is nice during development.
                     :taskmaster (make-instance
                                  (if async
                                      'hunchentoot:one-thread-per-connection-taskmaster
                                      'hunchentoot:single-threaded-taskmaster))))))

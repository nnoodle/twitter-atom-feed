#!sbcl --script
;;;; scraper.lisp
;;; small script to quickly scrape images from timelines

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(load "twitter-atom-feed.asd")
(ql:quickload :twitter-atom-feed :silent t)

(twitter-atom-feed::authenticate)

(uiop:run-program
 (append (uiop:split-string "wget -w 1 -P")
         (list (uiop:native-namestring "~/Downloads/scraped/") "--")
         (loop :for tweet :in (if (null (second uiop:*command-line-arguments*))
                                  (chirp:statuses/home-timeline :count 200)
                                  (chirp:statuses/user-timeline :screen-name (second uiop:*command-line-arguments*)
                                                                :count 200))
               :when (chirp::extended-entities tweet)
                 :append
                 (loop :for entity :in (cdr (assoc :media (chirp::extended-entities tweet)))
                       :collect (chirp:media-url-https entity))))
 :output t
 :error-output t)

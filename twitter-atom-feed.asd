;;;; twitter-atom-feed.asd

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(asdf:defsystem #:twitter-atom-feed
  :description "Twitter timeline to Atom feed"
  :author "Noodles!"
  :license "ISC"
  :version "0.3.1"

  :build-operation "program-op"
  :build-pathname "twitter-atom-feed"
  :entry-point "twitter-atom-feed:command-line"

  :serial t
  :depends-on (#:alexandria #:trivial-signal
               #:split-sequence #:memoize
               #:adopt #:chirp #:cl-ppcre
               #:hunchentoot #:easy-routes #:cl-who)
  :components ((:file "package")
               (:file "patches")
               (:file "config")
               (:file "server")
               (:file "filters")
               (:file "twitter-atom-feed")))

;;;; twitter-atom-feed.asd

(asdf:defsystem #:twitter-atom-feed
  :description "Twitter timeline to Atom feed"
  :author "Noodles!"
  :license "ISC"
  :version "0.3.0"

  :build-operation "program-op"
  :build-pathname "twitter-atom-feed"
  :entry-point "twitter-atom-feed:command-line"

  :serial t
  :depends-on (#:alexandria
               #:split-sequence #:memoize
               #:adopt #:chirp #:hunchentoot
               #:easy-routes #:cl-who #:cl-ppcre)
  :components ((:file "package")
               (:file "config")
               (:file "server")
               (:file "filters")
               (:file "twitter-atom-feed")))

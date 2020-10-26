;;;; twitter-atom-feed.asd

(asdf:defsystem #:twitter-atom-feed
  :description "Twitter timeline to Atom feed"
  :author "Noodles! <nnoodle@chiru.no>"
  :license "ISC"
  :version "0.0.1"

  :build-operation "program-op"
  :build-pathname "twitter-atom-feed"
  :entry-point "twitter-atom-feed:command-line"

  :serial t
  :depends-on (#:alexandria
               #:chirp #:unix-opts #:hunchentoot
               #:easy-routes #:cl-who #:cl-ppcre)
  :components ((:file "package")
               (:file "files")
               (:file "server")
               (:file "twitter-atom-feed")))

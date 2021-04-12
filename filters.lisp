
(in-package #:twitter-atom-feed-filters)

(setf (symbol-function 'image-p) #'chirp:extended-entities)
(setf (symbol-function 'retweet-p) #'chirp:retweet-p)
(setf (symbol-function 'nsfw-p) #'chirp:possibly-sensitive)

;;; a slightly less trivial example
;; (defun trigger-warning-p (tweet)
;;   "Tests if tweet begins with a 'trigger warning'."
;;   (let ((x (chirp:text tweet)))
;;     (and (>= (length x) 3)
;;          (string-equal (subseq x 0 3) "tw/"))))

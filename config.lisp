;;;; config.lisp

(in-package #:twitter-atom-feed)

;;; CLI options

(defvar *address* "localhost")
(defvar *port* 8080)
(defvar *memoize-tweets-p* nil)

;;; filesystem

(defvar *xdg-data* '()
  "Data from (read-data)")

(defun get-data ()
  "Get our data pathname"
  (uiop:xdg-config-home "twitter-atom-feed" "data.sexp"))

(defun get-config ()
  "Get our config pathname"
  (uiop:xdg-config-home "twitter-atom-feed" "config.lisp"))

(defun read-data ()
  "Read data from (get-data)"
  (if (not (null *xdg-data*))
      *xdg-data*
      (progn
        (ensure-directories-exist (get-data))
        (with-open-file (dat (get-data)
                             :direction :input
                             :if-does-not-exist :create)
          (handler-case (setf *xdg-data* (read dat))
            (end-of-file ()
              (with-open-file (dat (get-data)
                                   :direction :output
                                   :if-exists :overwrite)
                (format dat "()~%")
                nil)))))))

(defun set-data (&rest plist)
  "Set plist values in *xdg-data*."
  (if (oddp (length plist))
      (error "PLIST not even ~s" plist)
      (loop :for (key val) :on plist :by #'cddr
            :do (setf (getf *xdg-data* key) val))))

(defun write-data (&rest plist)
  "Write *xdg-data* to file."
  (unless (null plist)
    (apply #'set-data plist))
  (ensure-directories-exist (get-data))
  (with-open-file (dat (get-data)
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create)
    (write *xdg-data* :stream dat :pretty nil :readably t)))

(defun load-config ()
  (load (get-config) :if-does-not-exist nil))

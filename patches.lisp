;;; (incomplete) video-info patch
(in-package #:org.tymoonnext.chirp)

(defclass* media (entity)
  (url id display-url expanded-url start end sizes
       media-url media-url-https media-type source-status video-info) ; +
  (:documentation "Twitter media entity.

According to spec https://dev.twitter.com/docs/entities#The_media_entity"))

(defmethod make-entity ((type (eql :media)) parameters)
  (let ((id (cdr (assoc :id parameters)))
        (media-url (cdr (assoc :media-url parameters)))
        (media-url-https (cdr (assoc :media-url-https parameters)))
        (url (cdr (assoc :url parameters)))
        (display-url (cdr (assoc :display-url parameters)))
        (expanded-url (cdr (assoc :expanded-url parameters)))
        (type (cdr (assoc :type parameters)))
        (sizes (cdr (assoc :sizes parameters)))
        (source-status (cdr (assoc :source-status-id parameters)))
        (video-info (cdr (assoc :video-info parameters))) ; +
        (indices (cdr (assoc :indices parameters))))
    (make-instance 'media :id id :start (first indices) :end (second indices)
                          :media-url media-url :media-url-https media-url-https
                          :url url :display-url display-url :expanded-url expanded-url
                          :source-status source-status :media-type type
                          :video-info video-info ; +
                          :sizes (loop for (type . params) in sizes
                                       collect (cons type (make-entity type params))))))

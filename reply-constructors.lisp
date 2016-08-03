(in-package :ratatoskr)

;Simple replies
;--------------
(define-simple-reply-constructors
;RFC 1459
;--------
    (rpl-none "This shouldn't have happened.")
;   (defun rpl-userhost (replies &key prefix) …)
    )

;Specialized replies
;-------------------
;RFC 1459
;--------
(defun rpl-userhost (replies &key prefix)
  (declare
   (type (or list string) replies)
   (type (or prefix null) prefix))
  (make-instance 'rpl-userhost
		 :prefix prefix
		 :trailing (build-list-string (limit-targets 'cmd-userhost replies)
					      :delimiter #\Space)))

(in-package :ratatoskr)

;Reply utility functions/macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun define-simple-reply-constructor (type message &optional args)
    (declare
     (type symbol type)
     (type string message)
     (type (or integer boolean) args))
    (unless (subtypep (find-class type) (find-class 'message))
      (error 'type-error :datum (find-class type) :expected-type (find-class 'message)))
    (let (params)
      (when args
	(etypecase args
	  (boolean
	   (setf params (list 'param)))
	  (integer
	   (let ((*gensym-counter* 0))
	     (dotimes (n args)
	       (push (gensym "param-") params)))
	   (nreversef params))))
      `(defun ,type (target ,@params &key prefix)
	 (declare
	  ,@(when args `((type string ,@params)))
	  (type (or prefix null) prefix))
	 (make-instance ',type
			:prefix prefix
			:params (list target
				      ,@(when args params))
			:trailing ,message)))))

(defmacro define-simple-reply-constructors (&rest reply-specs)
  `(progn
     ,@(mapcar (lambda (reply-spec)
		 (apply #'define-simple-reply-constructor reply-spec))
	       reply-specs)))

(defun userhost-string (userhost)
  (concatenate 'string
	       (userhost-nick userhost)
	       (when (userhost-oper userhost) "*")
	       "="
	       (if (userhost-away userhost) "-" "+")
	       (userhost-host userhost)))

(defun whois-channel-string (channel)
  (declare
   (type whois-channel channel))
  (concatenate 'string
	       (when (whois-channel-op channel) "@")
	       (when (whois-channel-voice channel) "+")
	       (whois-channel-channel channel)))

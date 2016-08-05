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
	   (setf params (list 'target)))
	  (number
	   (let ((*gensym-counter* 0))
	     (dotimes (n args)
	       (push (gensym "target-0") params)))
	   (nreversef params))))
      `(defun ,type (,@params &key prefix)
	 (declare
	  ,@(when args `((type string ,@params)))
	  (type (or prefix null) prefix))
	 (make-instance ',type
			:prefix prefix
			,@(when args
				`(:params (list ,@params)))
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

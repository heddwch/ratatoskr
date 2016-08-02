(in-package :ratatoskr)

;Error utility functions/macros
(defmacro def-err (type message &key has-arg)
  (declare
   (type symbol type)
   (type string message)
   (type boolean has-arg))
  (unless (subtypep (find-class type) (find-class 'message))
    (error 'type-error))
  `(defun ,type (,@(when has-arg '(target)) &key prefix)
     (declare
      ,(when has-arg '(type string target))
      (type (or prefix null) prefix))
     (make-instance ',type
		    :prefix prefix
		    ,@(when has-arg
			    '(:params target))
		    :trailing ,message)))

;Errors

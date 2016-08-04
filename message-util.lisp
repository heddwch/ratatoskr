(in-package :ratatoskr)

(define-condition malformed-message (error) ())

;Message utility functions/macros
(defun targmax (object)
  (or (gethash (let ((class (etypecase object
			      (class object)
			      (symbol (find-class object))
			      (message (class-of object)))))
		 (unless (subtypep class (find-class 'message))
		   (error 'type-error
			  :datum class
			  :expected-type (find-class 'message)))
		 class)
	       *targmaxes*)
      1))

(defun (setf targmax) (value object)
  (setf (gethash (let ((class (etypecase object
				(class object)
				(symbol (find-class object))
				(message (class-of object)))))
		   (unless (subtypep class (find-class 'message))
		     (error 'type-error
			    :datum class
			    :expected-type (find-class 'message)))
		   class)
		 *targmaxes*)
	value))

(defun limit-targets (object targets)
  (declare
   (type (or symbol message class) object))
  (if (proper-list-p targets)
      (subseq targets
	      0
	      (min +max-targets+ *targmax* (list-length targets) (targmax object)))
      (list targets)))

(defun build-list-string (targets &key (delimiter ","))
  (declare
   (type (or list t) targets)
   (type (or string character) delimiter))
  (etypecase targets
    (string targets)
    (list (let ((target-string (first targets)))
	    (dolist (target (rest targets) target-string)
	      (declare (type string target))
	      (setf target-string
		    (concatenate 'string
				 target-string
				 (etypecase delimiter
				   (string delimiter)
				   (character (make-string 1
							   :initial-element
							   delimiter)))
				 target)))))))

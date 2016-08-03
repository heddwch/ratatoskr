(in-package :ratatoskr)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-targmax-classes (pairs)
    (mapcar (lambda (pair)
	      (cons (find-class (car pair)) (cdr pair)))
	    pairs)))

(defmacro set-default-targmaxes (&rest pairs)
  `(defparameter *targmaxes*
     (alist-hash-table
      ',(get-targmax-classes pairs)
      :test 'eq)))

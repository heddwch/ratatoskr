(defpackage ratatoskr
  (:use #:common-lisp #:usocket #:solipsism
	#:alexandria #:bordeaux-threads)
  (:export #:+max-params+
	   #:*params-per-message*))

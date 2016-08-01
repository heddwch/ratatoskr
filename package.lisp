(defpackage ratatoskr
  (:use #:common-lisp #:usocket #:solipsism
	#:alexandria #:bordeaux-threads)
  (:export #:+max-targets+
	   #:*targets-per-message*))

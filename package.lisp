(defpackage ratatoskr
  (:nicknames #:rat)
  (:use #:common-lisp #:usocket #:solipsism
	#:alexandria #:bordeaux-threads)
  (:export #:+max-params+ #:*params-per-message*
	   #:*command-map* #:command-mapping
	   #:make-prefix #:copy-prefix #:prefix-nick #:prefix-user #:prefix-host #:prefix-p
	   #:targmax))

(defpackage ratatoskr
  (:nicknames #:rat)
  (:use #:common-lisp #:usocket #:solipsism
	#:alexandria #:bordeaux-threads)
  (:export #:*command-map* #:command-mapping
	   #:make-prefix #:copy-prefix #:prefix-nick #:prefix-user #:prefix-host #:prefix-p
	   #:targmax #:+max-targets+ #:*targmax* #:*targmaxes*
	   #:make-mode #:mode-grant #:mode-mode
	   #:make-userhost #:userhost-nick #:userhost-oper #:userhost-away #:userhost-host
	   #:make-whois-channel #:whois-channel-channel #:whois-channel-op #:whois-channel-voice))

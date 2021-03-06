(in-package :asdf-user)

(defsystem "ratatoskr"
  :description "ratatoskr: Another rodent"
  :version "0.0.1"
  :author "Quinn Evans <yoshizuki@gmail.com>"
  :license "2-clause BSD"
  :depends-on (#:alexandria
	       #:usocket
	       #:flexi-streams
	       #:solipsism
	       #:bordeaux-threads)
  :components ((:file "package")
	       (:file "message-types" :depends-on ("package"))
	       (:file "config-util" :depends-on ("package"))
	       (:file "configuration" :depends-on ("package" "message-types" "config-util"))
	       (:file "message-util" :depends-on ("package" "message-types" "configuration"))
	       (:file "command-constructors" :depends-on ("message-types"))
	       (:file "reply-util" :depends-on ("package" "message-types"))
	       (:file "error-constructors" :depends-on ("package" "message-types" "reply-util"))
	       (:file "reply-constructors" :depends-on ("package" "message-types" "reply-util" "message-util"))))

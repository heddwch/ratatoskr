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
	       (:file "message-constructors" :depends-on ("message-types"))))

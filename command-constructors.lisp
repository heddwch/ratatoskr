(in-package :ratatoskr)

(define-condition malformed-message (error) ())

;Command utility functions/macros
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

(defun limit-targets (object list)
  (subseq list
	  0
	  (min +max-targets+ *targmax* (list-length list) (targmax object))))

(defun sort-targets-first (list &key stack-predicate stack-key)
  (let (with-target without-target)
    (map 'nil (lambda (pair)
		(if (cdr pair)
		    (push pair with-target)
		    (push pair without-target)))
	 list)
    (when stack-predicate
      (setf with-target (sort with-target stack-predicate :key stack-key))
      (setf without-target (sort without-target stack-predicate :key stack-key)))
    (append with-target without-target)))

(defun build-target-string (target-list)
  (let (target-string)
    (map 'nil (lambda (target)
		(setf target-string (concatenate 'string
						 target-string
						 (when target-string ",")
						 target)))
	 target-list)
    target-string))

;Commands
;--------
;RFC 1459
;--------
(defun cmd-pass (password)
  (declare
   (type string password))
  (make-instance (find-class 'cmd-pass) :params (list password)))

(defun cmd-nick (nick &key prefix hopcount)
  (declare
   (type string nick)
   (type (or prefix null) prefix)
   (type (or string number null) hopcount))
  (make-instance 'cmd-nick
		 :prefix prefix
		 :args (append (list nick)
			       (when hopcount
				 (list hopcount)))))

(defun cmd-user (username realname &key (hostname "bogus-host") (servername "bogus-server") prefix)
  (declare
   (type string username realname hostname servername)
   (type (or prefix null) prefix))
  (make-instance 'cmd-user
		 :prefix prefix
		 :params (list username
			       hostname
			       servername)
		 :trailing realname))

(defun cmd-server (servername hopcount info &key prefix)
  (declare
   (type string servername info)
   (type (or string number) hopcount)
   (type (or prefix null) prefix))
  (make-instance 'cmd-server
		 :prefix prefix
		 :params (list servername hopcount)
		 :trailing info))

(defun cmd-oper (user password)
  (declare (type string user password))
  (make-instance 'cmd-oper :params (list user password)))

(defun cmd-quit (&key quit-message prefix)
  (declare
   (type (or string null) quit-message)
   (type (or prefix null) prefix))
  (make-instance 'cmd-quit
		 :prefix prefix
		 :trailing quit-message))

(defun cmd-squit (server comment &key prefix)
  (declare
   (type string server comment)
   (type (or prefix null) prefix))
  (make-instance 'cmd-squit
		 :prefix prefix
		 :params (list server)
		 :trailing comment))

(defun cmd-join (channel-alist &key prefix)
  (declare
   (type list channel-alist)
   (type (or prefix null) prefix))
  (let (channel-string key-string)
    (let (channel-list key-list)
      (map 'nil (lambda (channel-pair)
		  (let ((channel (car channel-pair))
			(key (cdr channel-pair)))
		    (push channel channel-list)
		    (when key (push key key-list))))
	   (nreverse (sort-targets-first (limit-targets 'cmd-join channel-alist))))
      (setf channel-string (build-target-string channel-list))
      (setf key-string (build-target-string key-list)))
    (make-instance 'cmd-join
		   :prefix prefix
		   :params (list channel-string key-string))))

(defun cmd-part (channels &key part-message prefix)
  (declare
   (type list channels)
   (type (or string null) part-message)
   (type (or prefix null) prefix))
  (make-instance 'cmd-part
		 :prefix prefix
		 :params (list (build-target-string (limit-targets 'cmd-part channels)))
		 :trailing part-message))

(defun cmd-mode (target mode-alist &key prefix)
  (declare
   (type string target)
   (type list mode-alist)
   (type (or prefix null) prefix))
  (let* ((mode-alist (sort-targets-first (limit-targets 'cmd-mode mode-alist)
					 :stack-predicate (lambda (x1 x2)
							    (and x1 (not x2)))
					 :stack-key (lambda (pair)
						      (mode-grant (car pair)))))
	 (grant (not (mode-grant (caar mode-alist))))
	 mode-string
	 params)
    (map 'nil (lambda (pair)
		(let ((new-grant (mode-grant (car pair))))
		  (when (not (equalp grant new-grant))
		    (setf mode-string
			  (concatenate 'string
				       mode-string
				       (if new-grant "+" "-")))
		    (setf grant new-grant)))
		(setf mode-string
		      (concatenate 'string
				   mode-string
				   (mode-mode (car pair))))
		(push (cdr pair) params))
	 mode-alist)
    (make-instance 'cmd-mode
		   :prefix prefix
		   :params (append (list target mode-string)
				   (nreverse params)))))

(defun cmd-topic (channel &key topic prefix)
  (declare
   (type string channel)
   (type (or string null) topic)
   (type (or prefix null) prefix))
  (make-instance 'cmd-topic
		 :prefix prefix
		 :params (list channel)
		 :trailing topic))

(defun cmd-names (&key channels prefix)
  (declare
   (type (or list null) channels)
   (type (or prefix null) prefix))
  (let ((channel-string (build-target-string (limit-targets 'cmd-names channels))))
    (make-instance 'cmd-names
		   :prefix prefix
		   :params (when channel-string
			     (list channel-string)))))0

(defun cmd-list (&key channels server prefix)
  (declare
   (type (or list null) channels)
   (type (or string null) server)
   (type (or prefix null) prefix))
  (when server
    (unless channels
      (error 'malformed-message)))
  (let ((channel-string (build-target-string (limit-targets 'cmd-list channels))))
    (make-instance 'cmd-list
		   :prefix prefix
		   :params (append
			    (when channel-string
			      (list channel-string))
			    (when server
			      (list server))))))

(defun cmd-invite (nickname channel &key prefix)
  (declare
   (type string nickname channel)
   (type (or prefix null) prefix))
  (make-instance 'cmd-invite
		 :prefix prefix
		 :params (list nickname channel)))

(defun cmd-kick (channel user &key comment prefix)
  (declare
   (type string channel user)
   (type (or string null) comment)
   (type (or prefix null) prefix))
  (make-instance 'cmd-kick
		 :prefix prefix
		 :params (list channel user)
		 :trailing comment))

(defun cmd-version (&key server prefix)
  (declare
   (type (or string null) server)
   (type (or prefix null) prefix))
  (make-instance 'cmd-version
		 :prefix prefix
		 :params (when server
			   (list server))))

(defun cmd-stats (query &key server prefix)
  (declare
   (type character query)
   (type (or string null) server)
   (type (or prefix null) prefix))
  (make-instance 'cmd-stats
		 :prefix prefix
		 :params (append (list query)
				 (when server (list server)))))

(defun cmd-links (&key server server-mask prefix)
  (declare
   (type (or string null) server server-mask)
   (type (or prefix null) prefix))
  (make-instance 'cmd-links
		 :prefix prefix
		 :params (append (when server (list server))
				 (when server-mask (list server-mask)))))

(defun cmd-time (&key server-mask prefix)
  (declare
   (type (or string null) server-mask)
   (type (or prefix null) prefix))
  (make-instance 'cmd-time
		 :prefix prefix
		 :params (when server-mask (list server-mask))))

(defun cmd-connect (server &key port remote prefix)
  (declare
   (type string server)
   (type (or string number null) port)
   (type (or string null) remote)
   (type (or prefix null) prefix))
  (when remote
    (unless port
      (error 'malformed-message)))
  (make-instance 'cmd-connect
		 :prefix prefix
		 :params (append (list server)
				 (when port (list port))
				 (when remote (list remote)))))

(defun cmd-trace (&key server-mask prefix)
  (declare
   (type (or string null) server-mask)
   (type (or prefix null) prefix))
  (make-instance 'cmd-trace
		 :prefix prefix
		 :params (when server-mask (list server-mask))))

(defun cmd-admin (&key server-mask prefix)
  (declare
   (type (or string null) server-mask)
   (type (or prefix null) prefix))
  (make-instance 'cmd-admin
		 :prefix prefix
		 :params (when server-mask (list server-mask))))

(defun cmd-info (&key server-mask prefix)
  (declare
   (type (or string null) server-mask)
   (type (or prefix null) prefix))
  (make-instance 'cmd-info
		 :prefix prefix
		 :params (when server-mask (list server-mask))))

(defun cmd-privmsg (recipients message &key prefix)
  (declare
   (type list recipients)
   (type string message)
   (type (or prefix null) prefix))
  (make-instance 'cmd-privmsg
		 :prefix prefix
		 :params (list (build-target-string (limit-targets 'cmd-privmsg recipients)))
		 :trailing message))

(defun cmd-notice (recipients message &key prefix)
  (declare
   (type list recipients)
   (type string message)
   (type (or prefix null) prefix))
  (make-instance 'cmd-notice
		 :prefix prefix
		 :params (list (build-target-string (limit-targets 'cmd-notice recipients)))
		 :trailing message))

(defun cmd-who (&key name o prefix)
  (declare
   (type (or string null) name)
   (type boolean o)
   (type (or prefix null) prefix))
  (when o (unless name (error 'malformed-message)))
  (make-instance 'cmd-who
		 :prefix prefix
		 :params (when name
			   (append (list name)
				   (when o
				     (list "o"))))))

(defun cmd-whois (nickmasks &key server prefix)
  (declare
   (type list nickmasks)
   (type (or string null) server)
   (type (or prefix null) prefix))
  (when server
    (unless nickmasks
      (error 'malformed-message)))
  (make-instance 'cmd-whois
		 :prefix prefix
		 :params (append (when server
				   (list server))
				 (build-target-string
				  (limit-targets 'cmd-whois nickmasks)))))

(defun cmd-whowas (nick &key count server prefix)
  (declare
   (type string nick)
   (type (or string number null) count)
   (type (or string null) server)
   (type (or prefix null) prefix))
  (when server
    (unless count
      (error 'malformed-message)))
  (make-instance 'cmd-whowas
		 :prefix prefix
		 :params (append (list nick)
				 (when count (list count))
				 (when server (list server)))))

(defun cmd-kill (nick comment &key prefix)
  (declare
   (type string nick comment)
   (type (or prefix null) prefix))
  (make-instance 'cmd-kill
		 :prefix prefix
		 :params (list nick)
		 :trailing comment))

(defun cmd-ping (server-1 &key server-2 prefix)
  (declare
   (type string server-1)
   (type (or string null) server-2)
   (type (or prefix null) prefix))
  (make-instance 'cmd-ping
		 :prefix prefix
		 :parameters (append (list server-1)
				     (when server-2
				       (list server-2)))))

(defun cmd-pong (daemon &key daemon-2 prefix)
  (declare
   (type string daemon)
   (type (or string null) daemon-2)
   (type (or prefix null) prefix))
  (make-instance 'cmd-pong
		 :prefix prefix
		 :params (append (list daemon)
				 (when daemon-2
				   (list daemon-2)))))

(defun cmd-error (message &key prefix)
  (declare
   (type string message)
   (type (or prefix null) prefix))
  (make-instance 'cmd-error
		 :prefix prefix
		 :trailing message))

(defun cmd-away (&key message prefix)
  (declare
   (type (or string null) message)
   (type (or prefix null) prefix))
  (make-instance 'cmd-away
		 :prefix prefix
		 :trailing (when message
			     (list message))))

(defun cmd-rehash ()
  (make-instance 'cmd-rehash))

(defun cmd-restart ()
  (make-instance 'cmd-restart))

(defun cmd-summon (user &key server prefix)
  (declare
   (type string user)
   (type (or string null) server)
   (type (or prefix null) prefix))
  (make-instance 'cmd-summon
		 :prefix prefix
		 :params (append (list user)
				 (when server
				   (list server)))))

(defun cmd-users (&key server prefix)
  (declare
   (type (or string null) server)
   (type (or prefix null) prefix))
  (make-instance 'cmd-users
		 :prefix prefix
		 :params (when server
			   (list server))))

(defun cmd-wallops (message &key prefix)
  (declare
   (type string message)
   (type (or prefix null) prefix))
  (make-instance 'cmd-wallops
		 :prefix prefix
		 :trailing message))

(defun cmd-userhost (nicks &key prefix)
  (declare
   (type list nicks)
   (type (or prefix null) prefix))
  (unless (car nicks)
    (error 'malformed-message))
  (make-instance 'cmd-userhost
		 :prefix prefix
		 :params (limit-targets 'cmd-userhost nicks)))

(defun cmd-ison (nicks &key prefix)
  (declare
   (type list nicks)
   (type (or prefix null) prefix))
  (unless (car nicks)
    (error 'malformed-message))
  (make-instance 'cmd-ison
		 :prefix prefix
		 :params (limit-targets 'cmd-ison nicks)))

(defun err-nosuchnick (nick &key prefix)
  (declare
   (type string nick)
   (type (or prefix null) prefix))
  (make-instance 'err-nosuchnick
		 :prefix prefix
		 :params (list nick)
		 :trailing "No such nick/channel"))

(in-package :ratatoskr)

;Command utility functions/macros
(defun arg-to-sorted-pairs (parameters &key stack-predicate stack-key)
  (let (with-target without-target)
    (dolist (pair parameters)
      (typecase pair
	(cons
	 (if (cdr pair)
	     (push pair with-target)
	     (push pair without-target)))
	(t
	 (push (cons pair nil) without-target))))
    (when stack-predicate
      (setf with-target (sort with-target stack-predicate :key stack-key))
      (setf without-target (sort without-target stack-predicate :key stack-key)))
    (append with-target without-target)))


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
		 :args (if hopcount
			   (list nick hopcount)
			   (list nick))))

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

(defun cmd-join (channels &key prefix)
  (declare
   (type (or list string) channels)
   (type (or prefix null) prefix))
  (let (channel-string key-string)
    (let (channel-list key-list)
      (map 'nil (lambda (channel-pair)
		  (let ((channel (car channel-pair))
			(key (cdr channel-pair)))
		    (push channel channel-list)
		    (when key (push key key-list))))
	   (nreverse (arg-to-sorted-pairs (limit-targets 'cmd-join channels))))
      (setf channel-string (build-list-string channel-list))
      (setf key-string (build-list-string key-list)))
    (make-instance 'cmd-join
		   :prefix prefix
		   :params (if key-string
			       (list channel-string key-string)
			       (list channel-string)))))

(defun cmd-part (channels &key part-message prefix)
  (declare
   (type (or list string) channels)
   (type (or string null) part-message)
   (type (or prefix null) prefix))
  (make-instance 'cmd-part
		 :prefix prefix
		 :params (list (build-list-string (limit-targets 'cmd-part channels)))
		 :trailing part-message))

(defun cmd-mode (target modes &key prefix)
  (declare
   (type string target)
   (type (or list mode) modes)
   (type (or prefix null) prefix))
  (let* ((mode-alist (arg-to-sorted-pairs (limit-targets 'cmd-mode modes)
					 :stack-predicate (lambda (x1 x2)
							    (and x1 (not x2)))
					 :stack-key (lambda (pair)
						      (mode-grant (car pair)))))
	 (grant (not (mode-grant (caar mode-alist))))
	 mode-string
	 params)
    (map 'nil (lambda (pair)
		(let* ((mode (car pair))
		       (new-grant (mode-grant mode)))
		  (when (not (equalp grant new-grant))
		    (setf mode-string
			  (concatenate 'string
				       mode-string
				       (if new-grant "+" "-")))
		    (setf grant new-grant))
		  (setf mode-string
			(concatenate 'string
				     mode-string
				     (mode-mode mode)))
		  (when (cdr pair)
		    (push (cdr pair) params))))
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
   (type (or list string null) channels)
   (type (or prefix null) prefix))
  (let ((channel-string (build-list-string (limit-targets 'cmd-names channels))))
    (make-instance 'cmd-names
		   :prefix prefix
		   :params (when channel-string
			     (list channel-string)))))

(defun cmd-list (&key channels server prefix)
  (declare
   (type (or list string null) channels)
   (type (or string null) server)
   (type (or prefix null) prefix))
  (when server
    (unless channels
      (error 'malformed-message)))
  (let ((channel-string (build-list-string (limit-targets 'cmd-list channels))))
    (make-instance 'cmd-list
		   :prefix prefix
		   :params (when channel-string
			     (if server
				 (list channel-string server)
				 (list channel-string))))))

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
		 :params (if server
			     (list query server)
			     (list query))))

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
		 :params (if port
			     (if remote
				 (list server port remote)
				 (list server port))
			     (list server))))

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
   (type (or list string) recipients)
   (type string message)
   (type (or prefix null) prefix))
  (make-instance 'cmd-privmsg
		 :prefix prefix
		 :params (list (build-list-string (limit-targets 'cmd-privmsg recipients)))
		 :trailing message))

(defun cmd-notice (recipients message &key prefix)
  (declare
   (type (or list string) recipients)
   (type string message)
   (type (or prefix null) prefix))
  (make-instance 'cmd-notice
		 :prefix prefix
		 :params (list (build-list-string (limit-targets 'cmd-notice recipients)))
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
			   (if o
			       (list name "o")
			       (list name)))))

(defun cmd-whois (nickmasks &key server prefix)
  (declare
   (type (or list string) nickmasks)
   (type (or string null) server)
   (type (or prefix null) prefix))
  (when server
    (unless nickmasks
      (error 'malformed-message)))
  (let ((target-string (build-list-string (limit-targets 'cmd-whois nickmasks))))
    (make-instance 'cmd-whois
		 :prefix prefix
		 :params (if server
			     (list server target-string)
			     (list target-string)))))

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
		 :params (if count
			     (if server
				 (list nick count server)
				 (list nick count))
			     (list nick))))

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
		 :parameters (if server-2
				 (list server-1 server-2)
				 (list server-1))))

(defun cmd-pong (daemon &key daemon-2 prefix)
  (declare
   (type string daemon)
   (type (or string null) daemon-2)
   (type (or prefix null) prefix))
  (make-instance 'cmd-pong
		 :prefix prefix
		 :params (if daemon-2
			     (list daemon daemon-2)
			     (list daemon))))

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
		 :params (if server
			     (list user server)
			     (list user))))

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

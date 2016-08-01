(in-package :ratatoskr)

(define-condition malformed-message (error) ())

(defun limit-params (list)
  (subseq list 0 (min (list-length list)
			  (or *params-per-message*
			      +max-params+))))

(defun sort-params-first (list &key stack-predicate stack-key)
  (let (with-param without-param)
    (mapcar (lambda (pair)
	      (if (cdr pair)
		  (push pair with-param)
		  (push pair without-param)))
	    list)
    (when stack-predicate
      (setf with-param (sort with-param stack-predicate :key stack-key))
      (setf without-param (sort without-param stack-predicate :key stack-key)))
    (append with-param without-param)))

(defun cmd-pass (password)
  (declare
   (type string password))
  (make-instance (find-class 'cmd-pass) :params (list password)))

(defun cmd-nick (nick &key prefix hopcount)
  (declare
   (type string nick)
   (type (or prefix null) prefix)
   (type (or string number null) hopcount))
  (let ((options (list :params (list nick))))
    (when hopcount
      (nconc (getf options :params) (list hopcount)))
    (when prefix
      (setf (getf options :prefix) prefix))
    (apply #'make-instance 'cmd-privmsg options)))

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

(defun cmd-join (channel-plist &key prefix)
  (declare
   (type list channel-plist)
   (type (or prefix null) prefix))
  (let (channel-string key-string)
    (setf channel-plist (sort-params-first (limit-params channel-plist)))
    (mapcar (lambda (channel-pair)
	      (let ((channel (car channel-pair))
		    (key (cdr channel-pair)))
		(setf channel-string
		      (concatenate 'string
				   channel-string
				   (when channel-string ",")
				   channel))
		(when key
		  (setf key-string
			(concatenate 'string
				     key-string
				     (when key-string ",")
				     key)))))
	    channel-plist)
    (make-instance 'cmd-join
		   :prefix prefix
		   :params (list channel-string key-string))))

(defun cmd-part (channels &key part-message prefix)
  (declare
   (type list channels)
   (type (or string null) part-message)
   (type (or prefix null) prefix))
  (let (channel-string)
    (setf channels (limit-params channels))
    (dolist (channel channels)
      (setf channel-string
	    (concatenate 'string
			 channel-string
			 (when channel-string ",")
			 channel)))
    (make-instance 'cmd-part
		   :prefix prefix
		   :params (list channel-string)
		   :trailing part-message)))

(defun cmd-mode (target mode-plist &key prefix)
  (declare
   (type string target)
   (type list mode-plist)
   (type (or prefix null) prefix))
  (let* ((mode-plist (sort-params-first (limit-params mode-plist)
					  :stack-predicate (lambda (x1 x2)
							     (and x1 (not x2)))
					  :stack-key (lambda (pair)
						       (mode-grant (car pair)))))
	 (grant (not (mode-grant (caar mode-plist))))
	 mode-string
	 params)
    (mapcar
     (lambda (pair)
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
     mode-plist)
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
  (let (channel-string)
    (when channels
      (mapcar (lambda (channel)
		(when channel
		  (setf channel-string
			(concatenate 'string
				     channel-string
				     (when channel-string ",")
				     channel))))
	      (limit-params channels)))
    (make-instance 'cmd-names
		   :prefix prefix
		   :params (when channel-string
			     (list channel-string)))))

(defun cmd-list (&key channels server prefix)
  (declare
   (type (or list null) channels)
   (type (or string null) server)
   (type (or prefix null) prefix))
  (when server
    (unless channels
      (error 'malformed-message)))
  (let (channel-string)
    (when channels
      (dolist (channel channels)
	 (setf channel-string
	       (concatenate 'string
			    channel-string
			    (when channel-string ",")
			    channel))))
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

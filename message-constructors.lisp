(in-package :ratatoskr)

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

(defun cmd-join (channel-plist &optional prefix)
  (declare
   (type list channel-plist)
   (type (or prefix null) prefix))
  (let (channel-string key-string)
    (let ((limit (min (list-length channel-plist)
		    (or *targets-per-message*
			+max-targets+))))
      (setf channel-plist (subseq channel-plist 0 limit)))
    (let (with-keys without-keys)
      (mapcar (lambda (channel-pair)
		(let ((key (cdr channel-pair)))
		  (if key
		      (push channel-pair with-keys)
		      (push channel-pair without-keys))))
	      channel-plist)
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
	      (append with-keys without-keys)))
    (make-instance 'cmd-join
		   :prefix prefix
		   :params (list channel-string key-string))))

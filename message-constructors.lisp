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

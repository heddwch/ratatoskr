(in-package :ratatoskr)

;Error utility functions/macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun define-error-constructor (type message &optional has-arg)
    (declare
     (type symbol type)
     (type string message)
     (type boolean has-arg))
    (unless (subtypep (find-class type) (find-class 'message))
      (error 'type-error :datum (find-class type) :expected-type (find-class 'message)))
    `(defun ,type (,@(when has-arg '(target)) &key prefix)
       (declare
	,@(when has-arg '((type string target)))
	(type (or prefix null) prefix))
       (make-instance ',type
		      :prefix prefix
		      ,@(when has-arg
			      '(:params target))
		      :trailing ,message))))

;Errors
(defmacro define-error-constructors (&rest err-specs)
  `(progn
     ,@(mapcar (lambda (err-spec)
		 (apply #'define-error-constructor err-spec))
	       err-specs)))

(define-error-constructors
  (err-nosuchnick "No such nick/channel" t)
  (err-nosuchserver "No such server" t)
  (err-nosuchchannel "No such channel" t)
  (err-cannotsendtochan "Cannot send to channel" t)
  (err-toomanychannels "You have joined too many channels" t)
  (err-wasnosuchnick "There was no such nickname" t)
  (err-toomanytargets "Duplicate recipients. No message delivered" t)
  (err-noorigin "No origin specified")
  (err-notexttosend "No text to send")
  (err-notoplevel "No toplevel domain specified" t)
  (err-wildtoplevel "Wildcard in toplevel domain" t)
  (err-unknowncommand "Unknown command" t)
  (err-nomotd "MOTD File is missing")
  (err-noadmininfo "No administrative info available")
  )

(defun err-norecipient (command &key prefix)
  (declare
   (type (or string symbol class message) command)
   (type (or prefix null) prefix))
  (make-instance 'err-norecipient
		 :prefix prefix
		 :trailing (concatenate 'string
					"No recipient given ("
					(etypecase command
					  ((or symbol class message) (command-mapping command))
					  (string command))
					")")))

(defun err-fileerror (op file &key prefix)
  (declare
   (type string op file)
   (type (or prefix null) prefix))
  (make-instance 'err-fileerror
		 :prefix prefix
		 :trailing (concatenate 'string
					"File error doing "
					op
					" on "
					file)))

(in-package :ratatoskr)

(define-simple-reply-constructors
  (err-nosuchnick "No such nick/channel" t)
  (err-nosuchserver "No such server" t)
  (err-nosuchchannel "No such channel" t)
  (err-cannotsendtochan "Cannot send to channel" t)
  (err-toomanychannels "You have joined too many channels" t)
  (err-wasnosuchnick "There was no such nickname" t)
  (err-toomanytargets "Duplicate recipients. No message delivered" t)
  (err-noorigin "No origin specified")
; (defun err-norecipient (command &key prefix) …)
  (err-notexttosend "No text to send")
  (err-notoplevel "No toplevel domain specified" t)
  (err-wildtoplevel "Wildcard in toplevel domain" t)
  (err-unknowncommand "Unknown command" t)
  (err-nomotd "MOTD File is missing")
  (err-noadmininfo "No administrative info available" t)
; (defun err-fileerror (op file &key prefix) …)
  (err-nonicknamegiven "No nickname given")
  (err-erroneusnickname "Erroneous nickname" t)
  (err-nicknameinuse "Nickname is already in use" t)
  (err-nickcollision "Nickname collision KILL" t)
  (err-usernotinchannel "They aren't on that channel" 2)
  (err-notonchannel "You're not on that channel" t)
  (err-useronchannel "is already on channel" 2))

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

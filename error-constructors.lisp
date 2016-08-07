(in-package :ratatoskr)

;Simple errors
(define-simple-reply-constructors
;RFC 1459
;--------
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
  (err-useronchannel "is already on channel" 2)
  (err-nologin "User not logged in" t)
  (err-summondisabled "SUMMON has been disabled")
  (err-usersdisabled "USERS has been disabled")
  (err-notregistered "You have not registered")
  (err-needmoreparams "Not enough parameters" t)
  (err-alreadyregistered "You may not reregister")
  (err-nopermforhost "Your host isn't among the privileged")
  (err-passwdmismatch "Password incorrect")
  (err-yourebannedcreep "You are banned from this server")
  (err-keyset "Channel key already set" t)
  (err-channelisfull "Cannot join channel (+l)" t)
  (err-unknownmode "is unknown mode char to me" t)
  (err-inviteonlychan "Cannot join channel (+i)" t)
  (err-bannedfromchan "Cannot join channel (+b)" t)
  (err-badchannelkey "Cannot join channel (+k)" t)
  (err-noprivileges "Permission Denied- You're not an IRC operator")
  (err-chanoprivsneeded "You're not a channel operator" t)
  (err-cantkillserver "You can't kill a server!")
  (err-nooperhost "No O-lines for your host")
  (err-umodeunknownflag "Unknown MODE flag")
  (err-usersdontmatch "Cant change mode for other users"))

;Specialized errors
;------------------
;RFC 1459
;--------
(defun err-norecipient (target command &key prefix)
  (declare
   (type string target)
   (type (or string symbol class message) command)
   (type (or prefix null) prefix))
  (make-instance 'err-norecipient
		 :prefix prefix
		 :params (list target)
		 :trailing (concatenate 'string
					"No recipient given ("
					(etypecase command
					  ((or symbol class message) (command-mapping command))
					  (string command))
					")")))

(defun err-fileerror (target op file &key prefix)
  (declare
   (type string target op file)
   (type (or prefix null) prefix))
  (make-instance 'err-fileerror
		 :prefix prefix
		 :params (list target)
		 :trailing (concatenate 'string
					"File error doing "
					op
					" on "
					file)))

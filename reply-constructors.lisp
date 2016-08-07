(in-package :ratatoskr)

;Simple replies
;--------------
(define-simple-reply-constructors
;RFC 1459
;--------
  (rpl-none "This shouldn't have happened.")
; (defun rpl-userhost (replies &key prefix) …)
; (defun rpl-ison (nicks &key prefix) …)
; (defun rpl-away (nick message &key prefix) …)
  (rpl-unaway "You are no longer marked as being away")
  (rpl-nowaway "You have been marked as being away")
; (defun rpl-whoisuser (nick user host name &key prefix) …)
; (defun rpl-whoisserver (nick server server-info &key prefix) …)
  (rpl-whoisoperator "is an IRC operator" t)
; (defun rpl-whoisidle (nick seconds &key prefix) …)
  (rpl-endofwhois "End of /WHOIS list" t)
; (defun rpl-whoischannels (nick channels &key prefix) …)
; (defun rpl-whowasuser (nick user host name &key prefix)
  (rpl-endofwhowas "End of WHOWAS" t)
; (defun rpl-liststart (&key prefix) …)
; (defun rpl-list (channel population topic &key prefix) …)
  (rpl-listend "End of /LIST")
; (defun rpl-channelmodeis (channel modes &key mode-params prefix) …)
  (rpl-notopic "No topic is set" t)
; (defun rpl-topic (channel topic &key prefix) …)
; (defun rpl-inviting (channel nick &key prefix) …)
  (rpl-summoning "Summoning user to IRC" t)

  )

;Specialized replies
;-------------------
;RFC 1459
;--------
(defun rpl-userhost (target replies &key prefix)
  (declare
   (type string target)
   (type (or list userhost) replies)
   (type (or prefix null) prefix))
  (make-instance 'rpl-userhost
		 :prefix prefix
		 :params (list target)
		 :trailing (build-list-string
			    (mapcar #'userhost-string
				    (limit-targets 'cmd-userhost replies))
			    :delimiter #\Space)))

(defun rpl-ison (target nicks &key prefix)
  (declare
   (type string target)
   (type (or list string) nicks)
   (type (or prefix null) prefix))
  (make-instance 'rpl-ison
		 :prefix prefix
		 :params (list target)
		 :trailing (build-list-string (limit-targets 'cmd-ison nicks)
					      :delimiter #\Space)))

(defun rpl-away (target nick message &key prefix)
  (declare
   (type string target nick message)
   (type (or prefix null) prefix))
  (make-instance 'rpl-away
		 :prefix prefix
		 :params (list target nick)
		 :trailing message))

(defun rpl-whoisuser (target nick user host name &key prefix)
  (declare
   (type string target nick user host name)
   (type (or prefix null prefix)))
  (make-instance 'rpl-whoisuser
		 :prefix prefix
		 :params (list target nick user host "*")
		 :trailing name))

(defun rpl-whoisserver (target nick server server-info &key prefix)
  (declare
   (type string target nick server server-info)
   (type (or prefix null) prefix))
  (make-instance 'rpl-whoisserver
		 :prefix prefix
		 :params (list target nick server)
		 :trailing server-info))

(defun rpl-whoisidle (target nick seconds &key prefix)
  (declare
   (type string target nick)
   (type (or string integer) seconds)
   (type (or prefix null) prefix))
  (make-instance 'rpl-whoisidle
		 :prefix prefix
		 :params (list target nick seconds)
		 :trailing "seconds idle"))

(defun rpl-whoischannels (target nick channels &key prefix)
  (declare
   (type string target nick)
   (type list channels)
   (type (or prefix null) prefix))
  (make-instance 'rpl-whoischannels
		 :prefix prefix
		 :params (list target nick)
		 :trailing (build-list-string
			    (mapcar #'whois-channel-string
				    channels)
			    :delimiter #\Space)))

(defun rpl-whowasuser (target nick user host name &key prefix)
  (declare
   (type string target nick user host name)
   (type (or prefix null) prefix))
  (make-instance 'rpl-whowasuser
		 :prefix prefix
		 :params (list target nick user host "*")
		 :trailing name))

(defun rpl-liststart (target &key prefix)
  (declare
   (type string target)
   (type (or prefix null) prefix))
  (make-instance 'rpl-liststart
		 :prefix prefix
		 :params (list target "Channel")
		 :trailing ":Users  Name"))

(defun rpl-list (target channel population topic &key prefix)
  (declare
   (type string target channel)
   (type (or string integer) population)
   (type (or string null) topic)
   (type (or prefix null) prefix))
  (make-instance 'rpl-list
		 :prefix prefix
		 :params (list target channel population)
		 :trailing topic))

(defun rpl-channelmodeis (target channel modes &key mode-params prefix)
  (declare
   (type string target channel modes)
   (type (or list null) mode-params)
   (type (or prefix null) prefix))
  (let ((params (list target
		      channel
		      (concatenate 'string
				   "+"
				   modes))))
    (make-instance 'rpl-channelmodeis
		   :prefix prefix
		   :params (if mode-params
			       (append params
				       mode-params)
			       params))))

(defun rpl-topic (target channel topic &key prefix)
  (declare
   (type string target channel topic)
   (type (or prefix null) prefix))
  (make-instance 'rpl-topic
		 :prefix prefix
		 :params (list target channel)
		 :trailing topic))

(defun rpl-inviting (target channel nick &key prefix)
  (declare
   (type string target channel nick)
   (type (or prefix null) prefix))
  (make-instance 'rpl-inviting
		 :prefix prefix
		 :params (list target channel nick)))

(defun rpl-version (target version debuglevel server comments &key prefix)
  (declare
   (type string target version server comments)
   (type (or string integer null) debuglevel)
   (type (or prefix null) prefix))
  (make-instance 'rpl-version
		 :prefix prefix
		 :params (list target
			       (concatenate 'string
					    version
					    "."
					    (typecase debuglevel
					      (integer (write-to-string debuglevel))
					      (string debuglevel)))
			       server)
		 :trailing comments))

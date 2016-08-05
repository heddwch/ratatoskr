(in-package :ratatoskr)

(eval-when (:compile-toplevel)
  (defun init-message-type (msg)
    (let (type-sym command-string)
      (etypecase msg
	(symbol (setf command-string (symbol-name msg))
		(setf type-sym (intern (concatenate 'string "CMD-" command-string))))
	(cons (setf type-sym (car msg))
	      (setf command-string (cdr msg))))
      `((defclass ,type-sym (message) ())
	(setf (command-mapping ,command-string) (find-class ',type-sym))
	(setf (command-mapping (find-class ',type-sym)) ,command-string)
	(export ',type-sym)))))

(defstruct prefix
  nick
  user
  host)

(defclass message ()
  ((prefix :accessor prefix :initarg :prefix :initform nil)
   (params :accessor params :initarg :params :initform nil)
   (trailing :accessor trailing :initarg :trailing :initform nil)))

(defmethod print-object ((object message) stream)
  (print `(make-instance ',(class-name (class-of object))
			 ,@(when (slot-boundp object 'prefix)
				 `(:prefix ',(prefix object)))
			 ,@(when (slot-boundp object 'params)
				 `(:params ',(params object)))
			 ,@(when (slot-boundp object 'trailing)
				 `(:trailing ',(trailing object))))
	 stream))

(defstruct mode
  grant
  mode)

(defstruct userhost
  nick
  oper
  away
  host)

(defclass command-map ()
  ((line-to-command :initform (make-hash-table :test 'equalp))
   (command-to-line :initform (make-hash-table :test 'eq))))
(defgeneric command-mapping (key)
  (:method ((key string))
    (gethash key (slot-value *command-map* 'line-to-command)))
  (:method ((key symbol))
    (unless (subtypep (find-class key) (find-class 'message))
      (error 'type-error :datum (find-class key) :expected-type (find-class 'message)))
    (gethash (find-class key) (slot-value *command-map* 'command-to-line)))
  (:method ((key class))
    (unless (subtypep key (find-class 'message))
      (error 'type-error :datum key :expected-type (find-class 'message)))
    (gethash key (slot-value *command-map* 'command-to-line)))
  (:method ((key message))
    (gethash (class-of key) (slot-value *command-map* 'command-to-line))))
(defgeneric (setf command-mapping) (value key)
  (:method ((value class) (key string))
    (unless (subtypep value (find-class 'message))
      (error 'type-error :datum value :expected-type (find-class 'message)))
    (setf (gethash key (slot-value *command-map* 'line-to-command)) value))
  (:method ((value string) (key symbol))
    (unless (subtypep (find-class key) (find-class 'message))
      (error 'type-error :datum (find-class key) :expected-type (find-class 'message)))
    (setf (gethash (find-class key) (slot-value *command-map* 'command-to-line)) value))
  (:method ((value string) (key class))
    (unless (subtypep key (find-class 'message))
      (error 'type-error :datum key :expected-type (find-class 'message)))
    (setf (gethash key (slot-value *command-map* 'command-to-line)) value)))
(defvar *command-map* (make-instance 'command-map))
  
(defmacro init-message-types (&rest types)
  `(progn ,@(mapcan #'init-message-type types)))

(init-message-types
;RFC 1459
;-------- 
 PASS
 NICK
 USER
 SERVER
 OPER
 QUIT
 SQUIT
 JOIN
 PART
 MODE
 TOPIC
 NAMES
 LIST
 INVITE
 KICK
 VERSION
 STATS
 LINKS
 TIME
 CONNECT
 TRACE
 ADMIN
 INFO
 PRIVMSG
 NOTICE
 WHO
 WHOIS
 WHOWAS
 KILL
 PING
 PONG
 ERROR
 REHASH
 RESTART
 SUMMON
 USERS
 WALLOPS
 USERHOST
 ISON
 (ERR-NOSUCHNICK . "401")
 (ERR-NOSUCHSERVER . "402")
 (ERR-NOSUCHCHANNEL . "403")
 (ERR-CANNOTSENDTOCHAN . "404")
 (ERR-TOOMANYCHANNELS . "405")
 (ERR-WASNOSUCHNICK . "406")
 (ERR-TOOMANYTARGETS . "407")
 (ERR-NOORIGIN . "409")
 (ERR-NORECIPIENT . "411")
 (ERR-NOTEXTTOSEND . "412")
 (ERR-NOTOPLEVEL . "413")
 (ERR-WILDTOPLEVEL . "414")
 (ERR-UNKNOWNCOMMAND . "421")
 (ERR-NOMOTD . "422")
 (ERR-NOADMININFO . "423")
 (ERR-FILEERROR . "424")
 (ERR-NONICKNAMEGIVEN . "431")
 (ERR-ERRONEUSNICKNAME . "432")
 (ERR-NICKNAMEINUSE . "433")
 (ERR-NICKCOLLISION . "436")
 (ERR-USERNOTINCHANNEL . "441")
 (ERR-NOTONCHANNEL . "442")
 (ERR-USERONCHANNEL . "443")
 (ERR-NOLOGIN . "444")
 (ERR-SUMMONDISABLED . "445")
 (ERR-USERSDISABLED . "446")
 (ERR-NOTREGISTERED . "451")
 (ERR-NEEDMOREPARAMS . "461")
 (ERR-ALREADYREGISTERED . "462")
 (ERR-NOPERMFORHOST . "463")
 (ERR-PASSWDMISMATCH . "464")
 (ERR-YOUREBANNEDCREEP . "465")
 (ERR-KEYSET . "467")
 (ERR-CHANNELISFULL . "471")
 (ERR-UNKNOWNMODE . "472")
 (ERR-INVITEONLYCHAN . "473")
 (ERR-BANNEDFROMCHAN . "474")
 (ERR-BADCHANNELKEY . "475")
 (ERR-NOPRIVILEGES . "481")
 (ERR-CHANOPRIVSNEEDED . "482")
 (ERR-CANTKILLSERVER . "483")
 (ERR-NOOPERHOST . "491")
 (ERR-UMODEUNKNOWNFLAG . "501")
 (ERR-USERSDONTMATCH . "502")
 (RPL-NONE . "300")
 (RPL-USERHOST . "302")
 (RPL-ISON . "303")
 (RPL-AWAY . "301")
 (RPL-UNAWAY . "305")
 (RPL-NOWAWAY . "306")
 (RPL-WHOISUSER . "311")
 (RPL-WHOISSERVER . "312")
 (RPL-WHOISOPERATOR . "313")
 (RPL-WHOISIDLE . "317")
 (RPL-ENDOFWHOIS . "318")
 (RPL-WHOISCHANNELS . "319")
 (RPL-WHOWASUSER . "314")
 (RPL-ENDOFWHOWAS . "369")
 (RPL-LISTSTART . "321")
 (RPL-LIST . "322")
 (RPL-LISTEND . "323")
 (RPL-CHANNELMODEIS . "324")
 (RPL-NOTOPIC . "331")
 (RPL-TOPIC . "332")
 (RPL-INVITING . "341")
 (RPL-SUMMONING . "342")
 (RPL-VERSION . "351")
 (RPL-WHOREPLY . "352")
 (RPL-ENDOFWHO . "315")
 (RPL-NAMREPLY . "353")
 (RPL-ENDOFNAMES . "366")
 (RPL-LINKS . "364")
 (RPL-ENDOFLINKS . "365")
 (RPL-BANLIST . "367")
 (RPL-ENDOFBANLIST . "368")
 (RPL-INFO . "371")
 (RPL-ENDOFINFO . "374")
 (RPL-MOTDSTART . "375")
 (RPL-ENDOFMOTD . "376")
 (RPL-YOUREOPER . "381")
 (RPL-REHASHING . "382")
 (RPL-TIME . "391")
 (RPL-USERSSTART . "392")
 (RPL-USERS . "393")
 (RPL-ENDOFUSERS . "394")
 (RPL-NOUSERS . "395")
 (RPL-TRACELINK . "200")
 (RPL-TRACECONNECTING . "201")
 (RPL-TRACEHANDSHAKE . "202")
 (RPL-TRACEUNKNOWN . "203")
 (RPL-TRACEOPERATOR . "204")
 (RPL-TRACEUSER . "205")
 (RPL-TRACESERVER . "206")
 (RPL-RPL-TRACENEWTYPE . "208")
 (RPL-TRACELOG . "261")
 (RPL-STATSLINKINFO . "211")
 (RPL-STATSCOMMANDS . "212")
 (RPL-STATSCLINE . "213")
 (RPL-STATSNLINE . "214")
 (RPL-STATSILINE . "215")
 (RPL-STATSKLINE . "216")
 (RPL-STATSYLINE . "218")
 (RPL-ENDOFSTATS . "219")
 (RPL-STATSLLINE . "241")
 (RPL-STATSUPTIME . "242")
 (RPL-STATSOLINE . "243")
 (RPL-STATSHLINE . "244")
 (RPL-UMODEIS . "221")
 (RPL-LUSERCLIENT . "251")
 (RPL-LUSEROP . "252")
 (RPL-LUSERUNKNOWN . "253")
 (RPL-LUSERCHANNELS . "254")
 (RPL-LUSERME . "255")
 (RPL-ADMINME . "256")
 (RPL-ADMINLOC1 . "257")
 (RPL-ADMINLOC2 . "258")
 (RPL-ADMINEMAIL . "259")
 (RPL-TRACECLASS . "209")
 (RPL-STATSQLINE . "217")
 (RPL-SERVICEINFO . "231")
 (RPL-ENDOFSERVICES . "232")
 (RPL-SERVICE . "233")
 (RPL-SERVLIST . "234")
 (RPL-SERVLISTEND . "235")
 (RPL-WHOISCHANOP . "316")
 (RPL-KILLDONE . "361")
 (RPL-CLOSING . "362")
 (RPL-CLOSEEND . "363")
 (RPL-INFOSTART . "373")
 (RPL-MYPORTIS . "384")
 (ERR-YOUWILLBEBANNED . "466")
 (ERR-BADCHANMASK . "476")
 (ERR-NOSERVICEHOST . "492"))

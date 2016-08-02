(in-package :ratatoskr)

(default-targmaxes
    ((cmd-join . 4)
     (cmd-part . 4)
     (cmd-mode . 100)
     (cmd-names . 1)
     (cmd-privmsg . 4)
     (cmd-notice . 4)
     (cmd-whois . 4)
     (cmd-userhost . 5)))

(defparameter *targmaxes*
  (alist-hash-table +default-targmaxes+ :test 'eq))

(defconstant +max-params+ 10)
(defvar *targmax* 5)
(proclaim
 `(type (or (integer 1 ,+max-params+) null) *operands-per-message*))


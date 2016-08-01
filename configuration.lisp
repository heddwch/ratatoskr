(in-package :ratatoskr)

(default-targmaxes
  ((cmd-privmsg . 4)))

(defconstant +max-params+ 10)
(defvar *default-targmax* 4)
(proclaim
 `(type (or (integer 1 ,+max-params+) null) *operands-per-message*))


(in-package :ratatoskr)

(defconstant +max-params+ 10)
(defvar *params-per-message* 4)
(proclaim
 `(type (or (integer 1 ,+max-params+) null) *operands-per-message*))


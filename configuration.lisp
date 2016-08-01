(in-package :ratatoskr)

(defconstant +max-targets+ 20)
(defvar *targets-per-message* 4)
(proclaim
 `(type (or (integer 1 ,+max-targets+) null) *targets-per-message*))


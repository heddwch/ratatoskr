(in-package :ratatoskr)

(defconstant +max-targets+ 20)
(defvar *targets-per-message* 4)
(proclaim
 `(type (integer 1 ,+max-targets+) *targets-per-message*))


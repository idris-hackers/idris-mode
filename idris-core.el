;;; idris-core.el --- Core functionality

(require 'idris-compat)
(provide 'idris-core)

(defun idris-is-ident-char-p (ch)
  (or (and (<= ?a ch) (<= ch ?z))
      (and (<= ?A ch) (<= ch ?Z))
      (and (<= ?0 ch) (<= ch ?9))
      (= ch ?_)))

;;; idris-compat.el --- compatibility functions for Emacs 24.1

;; This file defines defvar-local, which was introduced in Emacs 24.3.

(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var))))

(provide 'idris-compat)

;;; idris-mode.el --- Major mode for editing Idris code

;; Copyright (C) 2013

;; Author:
;; Keywords: languages


;;; Commentary:

;; 

;;; Code:

(require 'idris-syntax)
(require 'inferior-idris-mode)

(define-derived-mode idris-mode fundamental-mode "Idris"
  (set-syntax-table idris-syntax-table)
  (idris-load-faces)
  (font-lock-add-keywords 'idris-mode
                          (mapcar (lambda (kwd) (cons kwd 'font-lock-keyword-face))
                                  idris-keywords)))

(provide 'idris-mode)
;;; idris-mode.el ends here

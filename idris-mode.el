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
  (set (make-local-variable 'font-lock-defaults)
       idris-font-lock-defaults))

; Automatically use idris-mode for .idr files.
(push '("\\.idr$" . idris-mode) auto-mode-alist)


(provide 'idris-mode)
;;; idris-mode.el ends here

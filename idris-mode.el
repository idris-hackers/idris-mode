;;; idris-mode.el --- Major mode for editing Idris code

;; Copyright (C) 2013

;; Author:
;; Keywords: languages


;;; Commentary:

;;

;;; Code:

(require 'idris-syntax)
(require 'inferior-idris-mode)

(defvar idris-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-l] 'inferior-idris-load-file)
    map)
  "Keymap used in Idris mode.")

(easy-menu-define idris-mode-menu idris-mode-map
  "Menu for the Idris major mode"
  `("Idris"
    ["Load file" inferior-idris-load-file]
    ;; TODO: customize-group
    ))


(define-derived-mode idris-mode fundamental-mode "Idris"
  "Major mode for Idris
     \\{text-mode-map}
We should run some hook at some point in the future."
  (set-syntax-table idris-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       idris-font-lock-defaults))

; Automatically use idris-mode for .idr files.
(push '("\\.idr$" . idris-mode) auto-mode-alist)

(provide 'idris-mode)
;;; idris-mode.el ends here

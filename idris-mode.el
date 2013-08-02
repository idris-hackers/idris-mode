;;; idris-mode.el --- Major mode for editing Idris code

;; Copyright (C) 2013

;; Author:
;; Keywords: languages


;;; Commentary:

;;

;;; Code:

(require 'idris-syntax)
(require 'idris-compat)
(require 'inferior-idris)
(require 'idris-repl)
(require 'idris-commands)
(require 'idris-warnings)
(require 'idris-prover)
(require 'idris-indentation)

(defgroup idris nil "Idris mode" :prefix 'idris)

(defcustom idris-interpreter-path "idris"
  "The path to the Idris interpreter"
  :type 'file
  :group 'idris)

(defcustom idris-interpreter-flags '()
  "The command line arguments passed to the Idris interpreter"
  :type '(repeat string)
  :group 'idris)

(defvar idris-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-l] 'idris-load-file)
    map)
  "Keymap used in Idris mode.")

(easy-menu-define idris-mode-menu idris-mode-map
  "Menu for the Idris major mode"
  `("Idris"
    ["Load file" idris-load-file t]
    ["Customize idris-mode" (customize-group 'idris) t]
    ["Quit inferior idris process" idris-quit t]
    ))

(defcustom idris-mode-hook '(turn-on-idris-indentation)
  "Hook to run upon entering Idris mode."
  :type 'hook
  :options '(turn-on-idris-indentation))

(define-derived-mode idris-mode fundamental-mode "Idris"
  "Major mode for Idris
     \\{idris-mode-map}
Invokes `idris-mode-hook'."
  :syntax-table idris-syntax-table
  :group 'idris
  (set (make-local-variable 'font-lock-defaults)
       idris-font-lock-defaults)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'comment-start) "--"))

; Automatically use idris-mode for .idr files.
(push '("\\.idr$" . idris-mode) auto-mode-alist)

(defun idris-quit ()
  (interactive)
  (let ((bufs (list :repl :process :proof-obligations :proof-shell :proof-script)))
    (dolist (b bufs)
      (let ((buf (get-buffer (idris-buffer-name b))))
	(when buf
	  (kill-buffer buf))))))

(provide 'idris-mode)
;;; idris-mode.el ends here

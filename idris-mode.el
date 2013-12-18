;;; idris-mode.el --- Major mode for editing Idris code

;; Copyright (C) 2013

;; Author:
;; URL: https://github.com/idris-hackers/idris-mode
;; Keywords: languages


;;; Commentary:

;; This is an Emacs mode for editing Idris code. It requires the latest
;; version of Idris, and some features may rely on the latest Git version of
;; Idris.

;;; Code:

(require 'idris-core)
(require 'idris-syntax)
(require 'idris-indentation)
(require 'idris-repl)
(require 'idris-commands)
(require 'idris-warnings)

(defgroup idris nil "Idris mode" :prefix 'idris :group 'languages)

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
    (define-key map (kbd "C-c C-l") 'idris-load-file)
    (define-key map (kbd "C-c C-t") 'idris-type-at-point)
    (define-key map (kbd "C-c C-c") 'idris-case-split)
    (define-key map (kbd "C-c C-m") 'idris-add-missing)
    (define-key map (kbd "C-c C-d") 'idris-add-clause)
    (define-key map (kbd "C-c C-w") 'idris-make-with-block)
    (define-key map (kbd "C-c C-a") 'idris-proof-search)
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

(defcustom idris-use-yasnippet-expansions t
  "Use yasnippet if available for completing interactive Idris commands"
  :type 'boolean)

;;;###autoload
(define-derived-mode idris-mode fundamental-mode "Idris"
  "Major mode for Idris
     \\{idris-mode-map}
Invokes `idris-mode-hook'."
  :syntax-table idris-syntax-table
  :group 'idris
  (set (make-local-variable 'font-lock-defaults)
       idris-font-lock-defaults)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'comment-start) "--")
  (setq mode-name `("Idris" (:eval (if (idris-current-buffer-dirty-p) " (Not loaded)" " (Loaded)")))))

;; Automatically use idris-mode for .idr files.
;;;###autoload
(push '("\\.idr$" . idris-mode) auto-mode-alist)

(defun idris-quit ()
  (interactive)
  (let* ((pbufname (idris-buffer-name :process))
         (pbuf (get-buffer pbufname)))
    (setq idris-currently-loaded-buffer nil)
    (if pbuf
        (progn
          (kill-buffer pbuf)
          (unless (get-buffer pbufname) (idris-kill-buffers)))
      (idris-kill-buffers))))

(defun idris-kill-buffers ()
  (idris-warning-reset-all)
  (let ((bufs (list :repl :proof-obligations :proof-shell :proof-script :log)))
    (dolist (b bufs)
      (let ((buf (get-buffer (idris-buffer-name b))))
        (when buf (kill-buffer buf))))))

(provide 'idris-mode)
;;; idris-mode.el ends here

;;; idris-mode.el --- Major mode for editing Idris code -*- lexical-binding: t -*-

;; Copyright (C) 2013

;; Author:
;; URL: https://github.com/idris-hackers/idris-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24"))


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
(require 'idris-common-utils)

(defgroup idris nil "Idris mode" :prefix 'idris :group 'languages)

(defcustom idris-interpreter-path "idris"
  "The path to the Idris interpreter"
  :type 'file
  :group 'idris)

(defcustom idris-interpreter-flags '()
  "The command line arguments passed to the Idris interpreter"
  :type '(repeat string)
  :group 'idris)

(defcustom idris-warnings-printing (list 'warnings-tree)
  "How to print warnings: tree view ('warnings-tree) in REPL ('warnings-repl)"
  :group 'idris
  :type '(repeat symbol)
  :options '(warnings-tree warnings-repl))

(defface idris-semantic-type-face
  '((t (:foreground "blue")))
  "The face to be used to highlight types"
  :group 'idris-faces)

(defface idris-semantic-data-face
  '((t (:foreground "red")))
  "The face to be used to highlight data and constructors"
  :group 'idris-faces)

(defface idris-semantic-function-face
  '((t (:foreground "green")))
  "The face to be used to highlight defined functions"
  :group 'idris-faces)

(defface idris-semantic-bound-face
  '((t (:foreground "purple")))
  "The face to be used to highlight bound variables"
  :group 'idris-faces)

(defface idris-semantic-implicit-face
  '((t (:slant italic)))
  "The face to be used to highlight implicit arguments"
  :group 'idris-faces)

(defvar idris-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'idris-load-file)
    (define-key map (kbd "C-c C-t") 'idris-type-at-point)
    (define-key map (kbd "C-c C-c") 'idris-case-split)
    (define-key map (kbd "C-c C-m") 'idris-add-missing)
    (define-key map (kbd "C-c C-d") 'idris-add-clause)
    (define-key map (kbd "C-c C-w") 'idris-make-with-block)
    (define-key map (kbd "C-c C-a") 'idris-proof-search)
    (define-key map (kbd "C-c _") 'idris-insert-bottom)
    map)
  "Keymap used in Idris mode.")

(easy-menu-define idris-mode-menu idris-mode-map
  "Menu for the Idris major mode"
  `("Idris"
    ["Load file" idris-load-file t]
    ["View compiler log" idris-view-compiler-log (get-buffer idris-log-buffer-name)]
    ["Quit inferior idris process" idris-quit t]
    "-----------------"
    ["Add initial match clause to type declaration" idris-add-clause t]
    ["Add missing cases" idris-add-missing t]
    ["Case split pattern variable" idris-case-split t]
    ["Add with block" idris-make-with-block t]
    ["Attempt to solve metavariable" idris-proof-search t]
    ["Display type" idris-type-at-point t]
    "-----------------"
    ["Customize idris-mode" (customize-group 'idris) t]
    ))

(defcustom idris-mode-hook '(turn-on-idris-indentation)
  "Hook to run upon entering Idris mode."
  :type 'hook
  :options '(turn-on-idris-indentation))

(defcustom idris-use-yasnippet-expansions t
  "Use yasnippet if available for completing interactive Idris commands"
  :type 'boolean)

;;;###autoload
(define-derived-mode idris-mode prog-mode "Idris"
  "Major mode for Idris
     \\{idris-mode-map}
Invokes `idris-mode-hook'."
  :syntax-table idris-syntax-table
  :group 'idris
  (set (make-local-variable 'font-lock-defaults)
       idris-font-lock-defaults)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'comment-start) "--")

  ; REPL completion for Idris source
  (set (make-local-variable 'completion-at-point-functions) '(idris-complete-symbol-at-point))

  ; Handle dirty-bit to avoid extra loads
  (add-hook 'first-change-hook 'idris-make-dirty)
  (setq mode-name `("Idris"
                    (:eval (if idris-rex-continuations "!" ""))
                    " "
                    (:eval (if (idris-current-buffer-dirty-p)
                               "(Not loaded)"
                             "(Loaded)")))))

;; Automatically use idris-mode for .idr files.
;;;###autoload
(push '("\\.idr$" . idris-mode) auto-mode-alist)

(defun idris-quit ()
  (interactive)
  (let* ((pbufname (idris-buffer-name :process))
         (pbuf (get-buffer pbufname)))
    (if pbuf
        (progn
          (kill-buffer pbuf)
          (unless (get-buffer pbufname) (idris-kill-buffers))
          (setq idris-rex-continuations nil))
      (idris-kill-buffers))))

(defun idris-kill-buffers ()
  (idris-warning-reset-all)
  (setq idris-currently-loaded-buffer nil)
  ; not killing :events since it it tremendously useful for debuging
  (let ((bufs (list :repl :proof-obligations :proof-shell :proof-script :log :info :notes)))
    (dolist (b bufs) (idris-kill-buffer b))))

(provide 'idris-mode)
;;; idris-mode.el ends here

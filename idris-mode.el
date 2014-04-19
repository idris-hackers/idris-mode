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
(require 'idris-settings)
(require 'idris-syntax)
(require 'idris-indentation)
(require 'idris-repl)
(require 'idris-commands)
(require 'idris-warnings)
(require 'idris-common-utils)
(require 'idris-ipkg-mode)


(defvar idris-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'idris-load-file)
    (define-key map (kbd "C-c C-t") 'idris-type-at-point)
    (define-key map (kbd "C-c C-d") 'idris-docs-at-point)
    (define-key map (kbd "C-c C-c") 'idris-case-split)
    (define-key map (kbd "C-c C-m") 'idris-add-missing)
    (define-key map (kbd "C-c C-s") 'idris-add-clause)
    (define-key map (kbd "C-c C-w") 'idris-make-with-block)
    (define-key map (kbd "C-c C-a") 'idris-proof-search)
    (define-key map (kbd "C-c C-r") 'idris-refine)
    (define-key map (kbd "C-c C-h C-a") 'idris-apropos)
    (define-key map (kbd "C-c _") 'idris-insert-bottom)
    (define-key map (kbd "C-c b") 'idris-ipkg-build)
    (define-key map (kbd "C-c c") 'idris-ipkg-clean)
    (define-key map (kbd "C-c i") 'idris-ipkg-install)
    map)
  "Keymap used in Idris mode.")

(easy-menu-define idris-mode-menu idris-mode-map
  "Menu for the Idris major mode"
  `("Idris"
    ["Load file" idris-load-file t]
    ["Compile and execute" idris-compile-and-execute]
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
    ["Build package" idris-ipkg-build t]
    ["Install package" idris-ipkg-install t]
    ["Clean package" idris-ipkg-clean t]
    "-----------------"
    ["Get documentation" idris-docs-at-point t]
    ["Apropos" idris-apropos t]
    "-----------------"
    ("Interpreter options" :active idris-process
     ["Show implicits" (idris-set-option :show-implicits t)
      :visible (not (idris-get-option :show-implicits))]
     ["Hide implicits" (idris-set-option :show-implicits nil)
      :visible (idris-get-option :show-implicits)]
     ["Show error context" (idris-set-option :error-context t)
      :visible (not (idris-get-option :error-context))]
     ["Hide error context" (idris-set-option :error-context nil)
      :visible (idris-get-option :error-context)])
    ["Customize idris-mode" (customize-group 'idris) t]
    ))


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

  ; Initialize the local options cache
  (idris-update-options-cache)

  ; REPL completion for Idris source
  (set (make-local-variable 'completion-at-point-functions) '(idris-complete-symbol-at-point))

  ; imenu support
  (set (make-local-variable 'imenu-case-fold-search) nil)
  (set (make-local-variable 'imenu-generic-expression)
       '(("Data" "^\\s-*data\\s-+\\(\\sw+\\)" 1)
         ("Data" "^\\s-*record\\s-+\\(\\sw+\\)" 1)
         ("Data" "^\\s-*codata\\s-+\\(\\sw+\\)" 1)
         ("Postulates" "^\\s-*postulate\\s-+\\(\\sw+\\)" 1)
         ("Classes" "^\\s-*class\\s-+\\(\\sw+\\)" 1)
         (nil "^\\s-*\\(\\sw+\\)\\s-*:" 1)
         ("Namespaces" "^\\s-*namespace\\s-+\\(\\sw\\|\\.\\)" 1)))

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

(provide 'idris-mode)
;;; idris-mode.el ends here

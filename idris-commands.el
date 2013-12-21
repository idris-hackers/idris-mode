;;; idris-commands.el --- Commands for Emacs passed to idris

;; Copyright (C) 2013 Hannes Mehnert

;; Author: Hannes Mehnert <hannes@mehnert.org>

;; License:
;; Inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
;; Therefore license is GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'idris-core)
(require 'inferior-idris)
(require 'idris-repl)
(require 'idris-warnings)
(require 'idris-compat)

(defvar-local idris-buffer-dirty-p t
  "An Idris buffer is dirty if there have been modifications since it was last loaded")

(defvar idris-currently-loaded-buffer nil
  "The buffer currently loaded by the running Idris")

(defun idris-make-dirty ()
  (setq idris-buffer-dirty-p t))

(defun idris-make-clean ()
  (setq idris-currently-loaded-buffer (current-buffer))
  (setq idris-buffer-dirty-p nil))

(defun idris-current-buffer-dirty-p ()
  "Check whether the current buffer's most recent version is loaded"
  (or idris-buffer-dirty-p
      (not (equalp (current-buffer)
                   idris-currently-loaded-buffer))))



(defun idris-ensure-process-and-repl-buffer ()
  "Ensures that an Idris process is running and the Idris REPL buffer exists"
  (idris-warning-reset)
  (idris-repl-buffer)
  (idris-run)
  (with-current-buffer (idris-repl-buffer)
    (idris-mark-output-start)))

(defun idris-load-file (notpop)
  "Pass the current buffer's file to the inferior Idris process."
  (interactive "P")
  (save-buffer)
  (idris-ensure-process-and-repl-buffer)
  (if (buffer-file-name)
      (when (idris-current-buffer-dirty-p)
        (idris-eval-async `(:load-file ,(buffer-file-name))
                          (apply-partially (lambda (notpop result)
                                             (unless notpop
                                               (pop-to-buffer (idris-repl-buffer)))
                                             (message result)) notpop))
        (idris-make-clean))
    (error "Cannot find file for current buffer")))

(defun idris-load-file-sync ()
  "Pass the current buffer's file synchronously to the inferior Idris process."
  (save-buffer)
  (idris-ensure-process-and-repl-buffer)
  (if (buffer-file-name)
      (when (idris-current-buffer-dirty-p)
        (idris-eval `(:load-file ,(buffer-file-name)))
        (idris-make-clean))
    (error "Cannot find file for current buffer")))


(defun idris-get-line-num ()
  "Get the current line number"
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun idris-thing-at-point ()
  "Return the line number and name at point"
  (let ((name (symbol-at-point))
        (line (idris-get-line-num)))
    (if name
        (cons (substring-no-properties (symbol-name name)) line)
      (error "Nothing identifiable under point"))))

(defun idris-type-at-point (thing)
  "Display the type of the name at point, considered as a global variable"
  (interactive "P")
  (let ((name (if thing (read-string "Check: ")
                (car (idris-thing-at-point)))))
    (when name
      (if thing (idris-ensure-process-and-repl-buffer) (idris-load-file-sync))
      (message "%s" (idris-eval `(:type-of ,name))))))

(defun idris-case-split ()
  "Case split the pattern variable at point"
  (interactive)
  (let ((what (idris-thing-at-point)))
    (when (car what)
      (idris-load-file-sync)
      (let ((result (idris-eval `(:case-split ,(cdr what) ,(car what)))))
        (delete-region (line-beginning-position) (line-end-position))
        (idris-insert-or-expand (substring result 0 (1- (length result))))))))

(defun idris-add-clause (proof)
  "Add clauses to the declaration at point"
  (interactive "P")
  (let ((what (idris-thing-at-point))
        (command (if proof :add-proof-clause :add-clause)))
    (when (car what)
      (idris-load-file-sync)
      (let ((result (idris-eval `(,command ,(cdr what) ,(car what)))))
        (forward-line 1)
        (idris-insert-or-expand result)))))

(defun idris-add-missing ()
  "Add missing cases"
  (interactive)
  (let ((what (idris-thing-at-point)))
    (when (car what)
      (idris-load-file-sync)
      (let ((result (idris-eval `(:add-missing ,(cdr what) ,(car what)))))
        (forward-line 1)
        (idris-insert-or-expand result)))))

(defun idris-make-with-block ()
  "Add with block"
  (interactive)
  (let ((what (idris-thing-at-point)))
    (when (car what)
      (idris-load-file-sync)
      (let ((result (idris-eval `(:make-with ,(cdr what) ,(car what)))))
        (beginning-of-line)
        (kill-line)
        (idris-insert-or-expand result)))))

(defun idris-insert-or-expand (str)
  "If yasnippet is loaded, use it to expand Idris compiler output, otherwise fall back on inserting the output"
  (if (and (fboundp 'yas-expand-snippet) idris-use-yasnippet-expansions)
      (let ((snippet (idris-metavar-to-snippet str)))
        (message snippet)
        (yas-expand-snippet snippet nil nil '((yas-indent-line nil))))
    (insert str)))

(defun idris-metavar-to-snippet (str)
  "Replace metavariables with yasnippet snippets"
  (lexical-let ((n 0))
    (cl-flet ((to-snippet-param (metavar)
                 (incf n)
                 (if (string= metavar "(_)")
                     (format "(${%s:_})" n)
                   (format "${%s:%s}" n metavar))))
      (replace-regexp-in-string "\\?[a-zA-Z0-9_]+\\|(_)" #'to-snippet-param str))))

(defun idris-proof-search (hints)
  "Invoke the proof search"
  (interactive "P")
  (let ((hints (if hints
                   (split-string (read-string "Hints: ") "[^a-zA-Z0-9']")
                 '()))
        (what (idris-thing-at-point)))
    (when (car what)
      (idris-load-file-sync)
      (let ((result (idris-eval `(:proof-search ,(cdr what) ,(car what) ,hints))))
        (save-excursion
          (let ((start (progn (search-backward "?") (point)))
                (end (progn (forward-char) (search-forward-regexp "[^a-zA-Z0-9_']") (backward-char) (point))))
            (delete-region start end))
          (idris-insert-or-expand result))))))

(provide 'idris-commands)

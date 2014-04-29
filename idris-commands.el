;;; idris-commands.el --- Commands for Emacs passed to idris -*- lexical-binding: t -*-

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
(require 'idris-settings)
(require 'inferior-idris)
(require 'idris-repl)
(require 'idris-warnings)
(require 'idris-compat)
(require 'idris-info)
(require 'idris-log)
(require 'idris-ipkg-mode)
(require 'idris-warnings-tree)
(require 'idris-metavariable-list)
(require 'cl-lib)

(defvar-local idris-buffer-dirty-p t
  "An Idris buffer is dirty if there have been modifications since it was last loaded")

(defvar idris-currently-loaded-buffer nil
  "The buffer currently loaded by the running Idris")

(defun idris-make-dirty ()
  (setq idris-buffer-dirty-p t))

(defun idris-make-clean ()
  (setq idris-buffer-dirty-p nil))

(defun idris-current-buffer-dirty-p ()
  "Check whether the current buffer's most recent version is loaded"
  (or idris-buffer-dirty-p
      (not (equal (current-buffer)
                  idris-currently-loaded-buffer))))


(defun idris-ensure-process-and-repl-buffer ()
  "Ensures that an Idris process is running and the Idris REPL buffer exists"
  (idris-repl-buffer)
  (idris-run)
  (with-current-buffer (idris-repl-buffer)
    (idris-mark-output-start)))

(defun idris-switch-working-directory (new-working-directory)
  (unless (string= idris-process-current-working-directory new-working-directory)
    (idris-ensure-process-and-repl-buffer)
    (idris-eval `(:interpret ,(concat ":cd " new-working-directory)))
    (setq idris-process-current-working-directory new-working-directory)))

(defun idris-list-metavariables-on-load ()
  "Use the user's settings from customize to determine whether to list the metavariables."
  (interactive)
  (when idris-metavariable-show-on-load (idris-list-metavariables)))

(defcustom idris-load-file-success-hook '(idris-list-metavariables)
  "Functions to call when loading a file is successful"
  :type 'hook
  :options '(idris-list-metavariables-on-load)
  :group 'idris)

(defun idris-load-file ()
  "Pass the current buffer's file to the inferior Idris process."
  (interactive)
  (save-buffer)
  (idris-ensure-process-and-repl-buffer)
  (if (buffer-file-name)
      (when (idris-current-buffer-dirty-p)
        (idris-warning-reset-all)
        (let* ((fn (buffer-file-name))
               (ipkg-srcdir (idris-ipkg-find-src-dir))
               (srcdir (if ipkg-srcdir
                           ipkg-srcdir
                         (file-name-directory fn))))
          (idris-switch-working-directory srcdir)
          (setq idris-currently-loaded-buffer nil)
          (idris-eval-async `(:load-file ,fn)
                          (lambda (result)
                            (idris-make-clean)
                            (setq idris-currently-loaded-buffer (current-buffer))
                            (when (member 'warnings-tree idris-warnings-printing)
                              (idris-list-compiler-notes))
                            (run-hooks 'idris-load-file-success-hook)
                            (message result))
                          (lambda (_condition)
                            (when (member 'warnings-tree idris-warnings-printing)
                              (idris-list-compiler-notes)
                              (pop-to-buffer (idris-buffer-name :notes)))))))
    (error "Cannot find file for current buffer")))

(defun idris-view-compiler-log ()
  "Jump to the log buffer, if it is open"
  (interactive)
  (let ((buffer (get-buffer idris-log-buffer-name)))
    (if buffer
        (pop-to-buffer buffer)
      (message "No Idris compiler log is currently open"))))

(defun idris-load-file-sync ()
  "Pass the current buffer's file synchronously to the inferior Idris process."
  (save-buffer)
  (idris-ensure-process-and-repl-buffer)
  (if (buffer-file-name)
      (when (idris-current-buffer-dirty-p)
        (idris-warning-reset-all)
        (let ((fn (buffer-file-name)))
          (idris-switch-working-directory (file-name-directory fn))
          (setq idris-currently-loaded-buffer nil)
          (idris-eval `(:load-file ,(file-name-nondirectory fn)))
          (setq idris-currently-loaded-buffer (current-buffer)))
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
  "Return the line number and name at point. Use this in Idris source buffers."
  (let ((name (symbol-at-point))
        (line (idris-get-line-num)))
    (if name
        (cons (substring-no-properties (symbol-name name)) line)
      (error "Nothing identifiable under point"))))

(defun idris-name-at-point ()
  "Return the name at point, taking into account semantic
annotations. Use this in Idris source buffers or in
compiler-annotated output. Does not return a line number."
  (let ((ref (get-text-property (point) 'idris-ref)))
    (if (null ref)
        (car (idris-thing-at-point))
      ref)))

(defun idris-info-for-name (what name)
  "Display the type for a name"
  (let* ((ty (idris-eval (list what name)))
             (result (car ty))
             (formatting (cdr ty)))
      (idris-show-info (format "%s" result) formatting)))


(defun idris-type-at-point (thing)
  "Display the type of the name at point, considered as a global variable"
  (interactive "P")
  (let ((name (if thing (read-string "Check: ")
                (idris-name-at-point))))
    (when name
      (idris-info-for-name :type-of name))))

(defun idris-who-calls-name (name)
  "Show the callers of NAME in a tree"
  (with-idris-info-buffer
   (insert "Callers\n")
   (let* ((callers (idris-eval `(:who-calls ,name)))
          (roots (mapcar #'(lambda (c) (idris-caller-tree c :who-calls)) (car callers))))
     (dolist (r roots) (idris-tree-insert r "")))
   (goto-char (point-min))))

(defun idris-who-calls-name-at-point (thing)
  (interactive "P")
  (let ((name (if thing (read-string "Who calls: ")
                (idris-name-at-point))))
    (when name
      (idris-who-calls-name name))))

(defun idris-name-calls-who (name)
  "Show the callees of NAME in a tree"
  (with-idris-info-buffer
   (insert "Callees\n")
   (let* ((callees (idris-eval `(:calls-who ,name)))
          (roots (mapcar #'(lambda (c) (idris-caller-tree c :calls-who)) (car callees))))
     (dolist (r roots) (idris-tree-insert r "")))
   (goto-char (point-min))))

(defun idris-name-calls-who-at-point (thing)
  (interactive "P")
  (let ((name (if thing (read-string "Calls who: ")
                (idris-name-at-point))))
    (when name
      (idris-name-calls-who name))))

(defun idris-caller-tree (caller cmd)
  "Display a tree from an IDESlave caller list, lazily retrieving a few levels at a time"
  (pcase caller
    (`((,name ,highlight) ,children)
     (make-idris-tree
      :item name
      :highlighting highlight
      :collapsed-p t
      :kids (lambda ()
              (cl-mapcan #'(lambda (child)
                             (let ((child-name (caar (idris-eval `(,cmd ,(car child))))))
                               (if child-name
                                   (list (idris-caller-tree child-name cmd))
                                 nil)))
                      children))))
    (t (error "failed to make tree from %s" caller))))

(defun idris-apropos (what)
  "Look up something in names, type signatures, and docstrings"
  (interactive "sSearch Idris docs for: ")
  (idris-info-for-name :apropos what))

(defun idris-docs-at-point (thing)
  "Display the internal documentation for the name at point, considered as a global variable"
  (interactive "P")
  (let ((name (if thing (read-string "Docs: ")
                (idris-name-at-point))))
    (when name
      (idris-info-for-name :docs-for name))))

(defun idris-case-split ()
  "Case split the pattern variable at point"
  (interactive)
  (let ((what (idris-thing-at-point)))
    (when (car what)
      (idris-load-file-sync)
      (let ((result (car (idris-eval `(:case-split ,(cdr what) ,(car what))))))
        (delete-region (line-beginning-position) (line-end-position))
        (idris-insert-or-expand (substring result 0 (1- (length result))))))))

(defun idris-add-clause (proof)
  "Add clauses to the declaration at point"
  (interactive "P")
  (let ((what (idris-thing-at-point))
        (command (if proof :add-proof-clause :add-clause)))
    (when (car what)
      (idris-load-file-sync)
      (let ((result (car (idris-eval `(,command ,(cdr what) ,(car what))))))
        (end-of-line)
        (insert "\n")
        (idris-insert-or-expand result)))))

(defun idris-add-missing ()
  "Add missing cases"
  (interactive)
  (let ((what (idris-thing-at-point)))
    (when (car what)
      (idris-load-file-sync)
      (let ((result (car (idris-eval `(:add-missing ,(cdr what) ,(car what))))))
        (forward-line 1)
        (idris-insert-or-expand result)))))

(defun idris-make-with-block ()
  "Add with block"
  (interactive)
  (let ((what (idris-thing-at-point)))
    (when (car what)
      (idris-load-file-sync)
      (let ((result (car (idris-eval `(:make-with ,(cdr what) ,(car what))))))
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

(defun idris-compile-and-execute ()
  "Execute the program in the current buffer"
  (interactive)
  (idris-load-file-sync)
  (idris-eval '(:interpret ":exec")))


(defun idris-metavar-to-snippet (str)
  "Replace metavariables with yasnippet snippets"
  (lexical-let ((n 0))
    (replace-regexp-in-string "\\?[a-zA-Z0-9_]+\\|(_)"
                              (lambda (metavar)
                                 (cl-incf n)
                                 (if (string= metavar "(_)")
                                     (format "(${%s:_})" n)
                                   (format "${%s:%s}" n metavar)))
                               str)))

(defun idris-proof-search (hints)
  "Invoke the proof search"
  (interactive "P")
  (let ((hints (if hints
                   (split-string (read-string "Hints: ") "[^a-zA-Z0-9']")
                 '()))
        (what (idris-thing-at-point)))
    (when (car what)
      (idris-load-file-sync)
      (let ((result (car (idris-eval `(:proof-search ,(cdr what) ,(car what) ,hints)))))
        (save-excursion
          (let ((start (progn (search-backward "?") (point)))
                (end (progn (forward-char) (search-forward-regexp "[^a-zA-Z0-9_']") (backward-char) (point))))
            (delete-region start end))
          (idris-insert-or-expand result))))))

(defun idris-refine (name)
  "Refine by some name, without recursive proof search"
  (interactive "MRefine by: ")
  (let ((what (idris-thing-at-point)))
    (unless (car what)
      (error "Could not find a metavariable at point to refine by"))
    (idris-load-file-sync)
    (let ((result (car (idris-eval `(:refine ,(cdr what) ,(car what) ,name)))))
      (save-excursion
        (let ((start (progn (search-backward "?") (point)))
              (end (progn (forward-char) (search-forward-regexp "[^a-zA-Z0-9_']") (backward-char) (point))))
          (delete-region start end))
        (idris-insert-or-expand result)))))

(defun idris-identifier-backwards-from-point ()
  (let ((identifier-start nil)
        (identifier-end (point))
        (last-char (char-before))
        (failure (list nil nil nil)))
    (if (idris-is-ident-char-p last-char)
        (progn
          (save-excursion
            (while (idris-is-ident-char-p (char-before))
              (backward-char))
            (setq identifier-start (point)))
          (if identifier-start
              (list (buffer-substring-no-properties identifier-start identifier-end)
                    identifier-start
                    identifier-end)
            failure))
      failure)))

(defun idris-complete-symbol-at-point ()
  "Attempt to complete the symbol at point as a global variable.

This function does not attempt to load the buffer if it's not
already loaded, as a buffer awaiting completion is probably not
type-correct, so loading will fail."
  (if (not idris-process)
      nil
    (cl-destructuring-bind (identifier start end) (idris-identifier-backwards-from-point)
      (when identifier
        (let ((result (car (idris-eval `(:repl-completions ,identifier)))))
          (cl-destructuring-bind (completions _partial) result
            (if (null completions)
                nil
              (list start end completions))))))))

(defun idris-insert-bottom ()
  "Insert _|_ at point"
  (interactive)
  (insert "_|_"))

(defun idris-list-metavariables ()
  "Get a list of currently-open metavariables"
  (interactive)
  (idris-metavariable-list-show (car (idris-eval '(:metavariables 80)))))

(defun idris-kill-buffers ()
  (idris-warning-reset-all)
  (setq idris-currently-loaded-buffer nil)
  ; not killing :events since it it tremendously useful for debuging
  (let ((bufs (list :repl :proof-obligations :proof-shell :proof-script :log :info :notes)))
    (dolist (b bufs) (idris-kill-buffer b))))

(defun idris-quit ()
  (interactive)
  (let* ((pbufname (idris-buffer-name :process))
         (pbuf (get-buffer pbufname)))
    (if pbuf
        (progn
          (kill-buffer pbuf)
          (unless (get-buffer pbufname) (idris-kill-buffers))
          (setq idris-rex-continuations '()))
      (idris-kill-buffers))))

(defun idris-make-ref-menu (name)
  (let ((menu (make-sparse-keymap)))
    (define-key menu [idris-ref-menu-get-type]
      `(menu-item "Get type"
                  (lambda () (interactive)))) ; x-popup-menu doesn't run cmds
    (define-key-after menu [idris-ref-menu-get-docs]
      `(menu-item "Get documentation"
                  (lambda () (interactive)))) ; x-popup-menu doesn't run cmds
    (define-key-after menu [idris-ref-menu-who-calls]
      `(menu-item "Who calls?"
                  (lambda () (interactive))))
    (define-key-after menu [idris-ref-menu-calls-who]
      `(menu-item "Calls who?"
                  (lambda () (interactive))))
    menu))

(defun idris-make-ref-menu-keymap (name)
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3]
      (lambda () (interactive)
        (let ((selection (x-popup-menu t (idris-make-ref-menu name))))
          (cond ((equal selection '(idris-ref-menu-get-type))
                 (idris-info-for-name :type-of name))
                ((equal selection '(idris-ref-menu-get-docs))
                 (idris-info-for-name :docs-for name))
                ((equal selection '(idris-ref-menu-who-calls))
                 (idris-who-calls-name name))
                ((equal selection '(idris-ref-menu-calls-who))
                 (idris-name-calls-who name))
                (t (message "%S" selection))))))
    map))


(provide 'idris-commands)

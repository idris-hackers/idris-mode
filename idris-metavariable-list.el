;;; idris-metavariable-list.el --- List Idris metavariables in a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2014 David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>

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

(require 'cl-lib)

(require 'idris-core)
(require 'idris-keys)
(require 'idris-warnings-tree)
(require 'idris-settings)

(defvar idris-metavariable-list-buffer-name (idris-buffer-name :metavariables)
  "The name of the buffer containing Idris metavariables")

(defun idris-metavariable-list-quit ()
  "Quit the Idris metavariable list"
  (interactive)
  (idris-kill-buffer idris-metavariable-list-buffer-name))

(defvar idris-metavariable-list-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "q") 'idris-metavariable-list-quit)
    (define-key map (kbd "RET") 'idris-compiler-notes-default-action-or-show-details)
    (define-key map (kbd "<mouse-2>") 'idris-compiler-notes-default-action-or-show-details/mouse)
    (cl-loop for keyer
             in '(idris-define-docs-keys
                  idris-define-general-keys
                  idris-define-active-term-keys)
             do (funcall keyer map))
    map))

(easy-menu-define idris-metavariable-list-mode-menu idris-metavariable-list-mode-map
  "Menu for the Idris metavariable list buffer"
  `("Idris Metavars"
    ["Show term interaction widgets" idris-add-term-widgets t]
    ["Close metavariable list buffer" idris-metavariable-list-quit t]))

(define-derived-mode idris-metavariable-list-mode fundamental-mode "Idris Metavars"
  "Major mode used for transient Idris metavariable list buffers
   \\{idris-metavariable-list-mode-map}
Invoces `idris-metavariable-list-mode-hook'.")

(defun idris-metavariable-list-buffer ()
  "Return the Idris metavariable buffer, creating one if there is not one"
  (get-buffer-create idris-metavariable-list-buffer-name))

(defun idris-metavariable-list-buffer-visible-p ()
  (if (get-buffer-window idris-metavariable-list-buffer-name 'visible) t nil))

(defun idris-metavariable-list-show (metavar-info)
  (if (null metavar-info)
      (progn (message "No metavariables found!")
             (idris-metavariable-list-quit))
    (with-current-buffer (idris-metavariable-list-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (idris-metavariable-list-mode)
      (when idris-show-help-text
        (insert "This buffer displays the unsolved metavariables from the currently-loaded code. ")
        (insert "Press the [P] buttons to solve the metavariables interactively in the prover.")
        (let ((fill-column 80))
          (fill-region (point-min) (point-max)))
        (insert "\n\n"))

      (insert "Metavariables:\n")
      (dolist (tree (mapcar #'idris-tree-for-metavariable metavar-info))
        (idris-tree-insert tree "")
        (insert "\n\n"))

      ;(idris-tree-insert root "")
      (insert "\n")
      (message "Press q to close")
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer (idris-metavariable-list-buffer))))

(defun idris-metavariable-tree-printer (tree)
  "Print TREE, formatted for metavariables."
  (idris-propertize-spans (idris-repl-semantic-text-props (idris-tree.highlighting tree))
    (insert (idris-tree.item tree)))
  (when (idris-tree.button tree)
    (insert " ")
    (apply #'insert-button (idris-tree.button tree))
    (insert (idris-tree.after-button tree))))


;;; Prevent circularity error
(autoload 'idris-prove-metavariable "idris-commands.el")

(defun idris-tree-for-metavariable (metavar)
  "Generate a tree for METAVAR.

METAVAR should be a three-element list consisting of the
metavariable name, its premises, and its conclusion."
  (cl-destructuring-bind (name premises conclusion) metavar
    (make-idris-tree :item name
                     :button `("[P]"
                               help-echo "Open in prover"
                               action ,#'(lambda (_)
                                           (interactive)
                                           (idris-prove-metavariable name)))
                     :highlighting `((0 ,(length name) ((:decor :metavar))))
                     :print-fn #'idris-metavariable-tree-printer
                     :collapsed-p (not idris-metavariable-list-show-expanded) ; from customize
                     :kids (list (idris-tree-for-metavariable-details name premises conclusion)))))

(defun idris-tree-for-metavariable-details (name premises conclusion)
  (let* ((name-width (1+ (apply #'max 0 (length name)
                                (mapcar #'(lambda (h) (length (car h)))
                                        premises))))
         (divider-marker nil)
         (contents (with-temp-buffer
                     (dolist (h premises)
                       (cl-destructuring-bind (name type formatting) h
                         (cl-dotimes (_ (- name-width (length name))) (insert " "))
                         (idris-propertize-spans (idris-repl-semantic-text-props
                                                  `((0 ,(length name) ((:decor :bound)))))
                           (insert name))
                         (insert " : ")
                         (idris-propertize-spans (idris-repl-semantic-text-props formatting)
                           (insert type))
                         (insert "\n")))
                     (setq divider-marker (point-marker))
                     (cl-destructuring-bind (type formatting) conclusion
                       (when premises
                         (insert " ")
                         (idris-propertize-spans (idris-repl-semantic-text-props
                                                  `((0 ,(length name) ((:decor :metavar)))))
                           (insert name))
                         (insert " : "))
                       (idris-propertize-spans (idris-repl-semantic-text-props formatting)
                         (insert type)))
                     (when premises
                       (let ((width (apply #'max 0
                                           (mapcar #'length
                                                   (split-string (buffer-string) "\n")))))
                         (goto-char (marker-position divider-marker))
                         (dotimes (_ (1+ width)) (insert "-"))
                         (insert "\n")))
                     (buffer-string))))
    (make-idris-tree :item contents
                     :active-p nil
                     :highlighting '())))


(provide 'idris-metavariable-list)

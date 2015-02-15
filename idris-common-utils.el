;;; idris-common-utils.el --- Useful utilities -*- lexical-binding: t -*-

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
(require 'cl-lib)


;;; These variables are here because many things depend on them
(defvar-local idris-buffer-dirty-p t
  "An Idris buffer is dirty if there have been modifications since it was last loaded")

(defvar idris-currently-loaded-buffer nil
  "The buffer currently loaded by the running Idris")

(defvar idris-loaded-region-overlay nil
  "The region loaded by Idris, should such a thing exist")

(defvar idris-process-current-working-directory ""
  "Working directory of Idris process")

(defvar idris-command-line-option-functions nil
  "A list of functions to call to compute the command-line arguments to Idris.
Each function should take no arguments and return a list of
strings that are suitable arguments to `start-process'.")

(defvar idris-mode-path nil
  "Directory containing the `idris-mode' package.
This is used to load resource files such as images.  The default
value is automatically computed from the location of the Emacs
Lisp package.")
(setq idris-mode-path (file-name-directory load-file-name))

(defun idris-buffer-name (type)
  (cl-assert (keywordp type))
  (concat (format "*idris-%s*" (substring (symbol-name type) 1))))

(defun idris-kill-buffer (buffer)
  (let ((buf (cond
              ((symbolp buffer)
               (get-buffer (idris-buffer-name buffer)))
              ((stringp buffer)
               (get-buffer buffer))
              ((bufferp buffer)
               buffer)
              (t (message "don't know how to kill buffer")))))
    (when (and buf (buffer-live-p buf)) (kill-buffer buf))))

(defun idris-minibuffer-respecting-message (text &rest args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((mtext (format " [%s]" (apply #'format text args))))
    (if (minibuffer-window-active-p (minibuffer-window))
        (minibuffer-message mtext)
      (message "%s" mtext))))

(defun idris-same-line-p (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (<= (max pos1 pos2) (line-end-position))))

(defmacro idris-save-marker (marker &rest body)
  "Save the contents of the marker MARKER while executing BODY."
  (declare (indent 1))
  (let ((pos (cl-gensym "pos")))
  `(let ((,pos (marker-position ,marker)))
     (prog1 (progn . ,body)
       (set-marker ,marker ,pos)))))

(defmacro idris-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (declare (indent 1))
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(defmacro idris-propertize-spans (spans &rest body)
  "Execute BODY and add the properties indicated by SPANS to the
inserted text (that is, relative to point prior to insertion)."
  (declare (indent 1))
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (cl-loop for (begin length props) in ,spans
                  do (add-text-properties (+ ,start begin)
                                          (+ ,start begin length)
                                          props))))))

;;; Take care of circular dependency issue
(autoload 'idris-make-ref-menu-keymap "idris-commands.el")
(autoload 'idris-make-metavariable-keymap "idris-commands.el")
(autoload 'idris-make-error-keymap "idris-commands.el")
(autoload 'idris-eval "inferior-idris.el")

(defun idris-repl-semantic-text-props (highlighting)
  (cl-loop for (start length props) in highlighting
           collecting (list start
                            length
                            (let* ((name (assoc :name props))
                                   (implicit (assoc :implicit props))
                                   (decor (assoc :decor props))
                                   (term (assoc :tt-term props))
                                   (unique-val (cl-gensym)) ; HACK to stop consecutive mouse-faces from interfering
                                   (implicit-face (if (and implicit (equal (cadr implicit) :True))
                                                      '(idris-semantic-implicit-face)
                                                    nil))
                                   (text-face (pcase (assoc :text-formatting props)
                                                (`(:text-formatting :bold)
                                                 '(bold))
                                                (`(:text-formatting :italic)
                                                 '(italic))
                                                (`(:text-formatting :underline)
                                                 '(underline))
                                                (_ nil)))
                                   (decor-face (if decor
                                                   (cdr (assoc (cadr decor)
                                                               '((:type idris-semantic-type-face)
                                                                 (:data idris-semantic-data-face)
                                                                 (:function idris-semantic-function-face)
                                                                 (:keyword idris-keyword-face)
                                                                 (:metavar idris-metavariable-face)
                                                                 (:bound idris-semantic-bound-face))))
                                                 nil))
                                   (doc-overview (pcase (assoc :doc-overview props)
                                                   (`(:doc-overview ,docs) (concat "\n" docs))
                                                   (_ "")))
                                   (type (pcase (assoc :type props)
                                           (`(:type ,ty) (concat " : " ty))
                                           (_ "")))
                                   (idris-err (assoc :error props))
                                   (err-face (if idris-err
                                                 '(idris-warning-face)
                                               ()))
                                   (mousable-face
                                    (cond ((and (not (equal (cadr decor) :bound)) ;non-bound becomes clickable
                                                name)
                                           `((:inherit ,decor-face :box t :hack ,unique-val)))
                                          (idris-err
                                           `((:inherit ('idris-warning-face highlight))))
                                          (t nil)))
                                   (mouse-help
                                    (cond ((and (not (equal (cadr decor) :bound)) ;non-bound becomes clickable
                                                name)
                                           "\n<mouse-3> context menu")
                                          (idris-err (idris-eval `(:error-string ,(cadr idris-err))))
                                          (t ""))))
                              `(rear-nonsticky t
                                ,@(if name
                                      (append `(help-echo (concat ,(cadr name)
                                                                  ,type
                                                                  ,doc-overview
                                                                  ,mouse-help))
                                              (cond ((and (not (member (cadr decor) '(:bound :metavar)))
                                                          name)
                                                     `(idris-ref ,(cadr name)
                                                       keymap ,(idris-make-ref-menu-keymap (cadr name))))
                                                    ((equal (cadr decor) :metavar)
                                                     `(idris-ref ,(cadr name)
                                                       keymap ,(idris-make-metavariable-keymap (cadr name))))
                                                    (t nil)))
                                    nil)
                                ,@(if mousable-face
                                      (list 'mouse-face mousable-face)
                                    ())
                                ,@(if term
                                      (list 'idris-tt-term (cadr term))
                                    ())
                                ,@(if idris-err
                                      `(idris-tt-error ,(cadr idris-err)
                                        help-echo ,@mouse-help
                                        keymap ,(idris-make-error-keymap (cadr idris-err)))
                                    ())
                                ,@(let ((f (append text-face
                                                   implicit-face
                                                   decor-face
                                                   err-face)))
                                    (if f (list 'face f) ())))))))

;;; Was originally slime-search-property - thanks SLIME!
(defun idris-search-property (prop &optional backward prop-value-fn)
  "Search for the next text range where PROP is non-nil.
Return the value of PROP, or nil if it is not found.
If BACKWARD is non-nil, search backward.
If PROP-VALUE-FN is non-nil use it to extract PROP's value."
  (let ((next-candidate (if backward
                            #'previous-single-char-property-change
                          #'next-single-char-property-change))
        (prop-value-fn (or prop-value-fn
                            (lambda ()
                              (get-text-property (point) prop))))
        (start (point))
        (prop-value))
    (while (progn
             (goto-char (funcall next-candidate (point) prop))
             (not (or (setq prop-value (funcall prop-value-fn))
                      (eobp)
                      (bobp)))))
    (cond (prop-value)
          (t (goto-char start) nil))))

;;; Dispatching of events and helpers
(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `cl-case' and `cl-destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (declare (indent 1))
  (let ((operator (cl-gensym "op-"))
	(operands (cl-gensym "rand-"))
	(tmp (cl-gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (cl-destructuring-bind ((op &rest rands) &rest body) clause
                         `(,op (destructuring-bind ,rands ,operands
                                 . ,body)))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "ELISP destructure-case failed: %S" ,tmp))))))))

(defun idris-lidr-p (&optional buffer)
  "Return t if BUFFER is a literate Idris file, or nil otherwise. Use the current buffer if
BUFFER is not supplied or is nil."
  (string= (file-name-extension (buffer-file-name buffer)) "lidr"))

(defun idris-make-file-link-overlay (start end keymap help-echo)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'idris-file-link t)
    (overlay-put overlay 'keymap keymap)
    (overlay-put overlay 'mouse-face 'highlight)
    (overlay-put overlay 'help-echo help-echo)))

(defun idris-clear-file-link-overlays (&optional mode)
  "Remove all file link overlays from the current buffer"
  (when (or (not mode) (eq major-mode mode))
    (remove-overlays (point-min) (point-max) 'idris-file-link t)))

(defun idris-make-module-link (start end src-dir)
  "Attempt to make the region between START and END into a
clickable link to open a module for editing, with modules located
relative to SRC-DIR"
  (let* ((name (buffer-substring-no-properties start end))
         (fname (split-string name "\\."))
         (basename (concat (mapconcat 'file-name-as-directory (cons src-dir (butlast fname)) "")
                           (car (last fname))))
         (idr (concat basename ".idr"))
         (lidr (concat basename ".lidr")))
    (cl-flet ((make-link (src-name)
                 (let ((map (make-sparse-keymap)))
                   (define-key map [mouse-2] #'(lambda ()
                                                 (interactive)
                                                 (find-file src-name)))
                   (idris-make-file-link-overlay start end map "mouse-2: edit module"))))
      (if (file-exists-p idr)
          (make-link idr)
        (when (file-exists-p lidr)
          (make-link lidr))))))


(provide 'idris-common-utils)

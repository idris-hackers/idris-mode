;;; idris-warnings.el --- Mark warnings reported by idris in buffers -*- lexical-binding: t -*-

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
(require 'idris-common-utils)
(require 'cl-lib)

(defface idris-warning-face
  `((((class color) (background light))
     (:underline "orange"))
    (((class color) (background dark))
     (:underline "coral"))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'idris-faces)

(defvar idris-warnings-buffers '() "All buffers which have warnings")
(defvar-local idris-warnings '() "All warnings in the current buffer")
(defvar idris-raw-warnings '() "All warnings from Idris")

(defun idris-warning-event-hook-function (event)
  (pcase event
    (`(:warning ,output ,_target)
     (idris-warning-overlay output)
     t)
    (_ nil)))

(defun idris-warning-reset-all ()
  (mapc #'idris-warning-reset-buffer idris-warnings-buffers)
  (setq idris-raw-warnings '())
  (setq idris-warnings-buffers '()))

(defun idris-warning-reset-buffer (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (idris-warning-reset))))

(defun idris-warning-reset ()
  (mapc #'delete-overlay idris-warnings)
  (setq idris-warnings '())
  (delq (current-buffer) idris-warnings-buffers))

(defun get-region (line)
  (goto-char (point-min))
  (cl-values
   (line-beginning-position line)
   (line-end-position line)))

(defun idris-warning-overlay-p (overlay)
  (overlay-get overlay 'idris-warning))

(defun idris-warning-overlay-at-point ()
  "Return the overlay for a note starting at point, otherwise nil."
  (cl-find (point) (cl-remove-if-not 'idris-warning-overlay-p (overlays-at (point)))
        :key 'overlay-start))

(defun idris-warning-overlay (warning)
  "Add a compiler warning to the buffer as an overlay.
May merge overlays, if there's already one in the same location.
WARNING is of form (filename linenumber column message &optional highlighting-spans)
or the old format, used by Idris up to 0.9.10.1, which does not contain a column
Since March 10th 2014 (commit 7437ebe5052250630ca52117dd50dbf3187807d5) - Idris 0.9.11.2
WARNING is of form (filename (startline startcolumn) (endline endcolumn) message &optional highlighting-spans)
"
  (cl-case (safe-length warning)
    (3 (cl-destructuring-bind (filename lineno message) warning
         (idris-real-warning-overlay filename lineno 0 message)))
    (4 (cl-destructuring-bind (filename lineno col message) warning
         (idris-real-warning-overlay filename lineno col message)))
    (5 (cl-destructuring-bind (filename sl1 sl2 message highlighting) warning
         (if (listp sl1)
             (progn
               (cl-assert (listp sl2))
               (cl-assert (eq (safe-length sl1) 2))
               (cl-assert (eq (safe-length sl2) 2))
               (idris-real-warning-overlay filename (nth 0 sl1) (nth 1 sl1) message highlighting))
           (cl-assert (integerp sl1))
           (cl-assert (integerp sl2))
           (idris-real-warning-overlay filename sl1 sl2 message highlighting))))))

(defun idris-real-warning-overlay (filename lineno col message &optional spans)
  "Add the compiler warning to the buffer for real!"
  (push (list filename lineno col message spans) idris-raw-warnings)
  (let* ((fullpath (concat (file-name-as-directory idris-process-current-working-directory)
                          filename))
         (buffer (get-file-buffer fullpath)))
    (when (not (null buffer))
      (with-current-buffer buffer
        (cl-multiple-value-bind (start end) (get-region lineno)
          (goto-char start)
          ; this is a hack to have warnings reported which point to empty lines
          (let ((rend (if (equal start end)
                          (progn (insert " ")
                                 (1+ end))
                        end)))
            (let ((overlay (idris-warning-overlay-at-point)))
              (if overlay
                  (idris-warning-merge-overlays overlay message)
                (idris-warning-create-overlay (+ start col) rend message)))))))))

(defun idris-warning-merge-overlays (overlay message)
  (overlay-put overlay 'help-echo
               (concat (overlay-get overlay 'help-echo) "\n" message)))

(defun idris-warning-create-overlay (start end message)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'idris-warning message)
    (overlay-put overlay 'help-echo message)
    (overlay-put overlay 'face 'idris-warning-face)
    (overlay-put overlay 'mouse-face 'highlight)
    (push overlay idris-warnings)
    (unless (memq (current-buffer) idris-warnings-buffers)
      (push (current-buffer) idris-warnings-buffers))
    overlay))

(provide 'idris-warnings)



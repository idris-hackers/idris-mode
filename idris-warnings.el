;;; idris-warnings.el --- Mark warnings reported by idris in buffers

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

(defface idris-warning-face
  `((((class color) (background light))
     (:underline "orange"))
    (((class color) (background dark))
     (:underline "coral"))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'idris-faces)

(defvar-local idris-warnings '() "All warnings in the current buffer")

(defun idris-warning-event-hook-function (event)
  (destructure-case event
    ((:warning output target)
     (idris-warning-overlay output)
     t)
    (t nil)))

(defun idris-warning-reset ()
  (mapc #'delete-overlay idris-warnings)
  (setq idris-warnings '()))

(defun get-region (line)
  (goto-char (point-min))
  (values
   (line-beginning-position line)
   (1- (line-beginning-position (1+ line)))))

(defun idris-warning-overlay-p (overlay)
  (overlay-get overlay 'idris-warning))

(defun idris-warning-overlay-at-point ()
  "Return the overlay for a note starting at point, otherwise nil."
  (find (point) (remove-if-not 'idris-warning-overlay-p (overlays-at (point)))
        :key 'overlay-start))

(defun idris-warning-overlay (warning)
  "Add a compiler warning to the buffer as an overlay.
May merge overlays, if there's already one in the same location.
WARNING is of form (filename linenumber message)"
  (destructuring-bind (filename lineno message) warning
    (let ((buffer (get-file-buffer filename)))
      (when (not (null buffer))
        (with-current-buffer buffer
          (multiple-value-bind (start end) (get-region lineno)
            (when start
              (goto-char start)
              (let ((overlay (idris-warning-overlay-at-point)))
                (if overlay
                    (idris-warning-merge-overlays overlay message)
                  (idris-warning-create-overlay start end message))))))))))

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
    overlay))

(provide 'idris-warnings)



;;; idris-events.el --- Logging of events in inferior Idris

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

(require 'idris-common-utils)

(defvar idris-event-buffer-name (idris-buffer-name :events)
  "The name of the Idris event buffer.")

(defun idris-events-buffer ()
  "Return or create the event log buffer."
  (or (get-buffer idris-event-buffer-name)
      (let ((buffer (get-buffer-create idris-event-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (set (make-local-variable 'outline-regexp) "^(")
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) "")
          (setq buffer-read-only t))
        buffer)))

(defun idris-log-event (event sending)
  "Record the fact that EVENT occured."
  (with-current-buffer (idris-events-buffer)
    (goto-char (point-max))
    (let ((buffer-read-only nil)
          (time (substring (number-to-string (float-time)) 0 14)))
      (save-excursion
        (if sending
            (insert (concat time " -> "))
          (insert (concat time " <- ")))
        (idris-pprint-event event (current-buffer))))
    (goto-char (point-max))))

(defun idris-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER."
  (let ((print-length 20)
        (print-level 6)
        (pp-escape-newlines t))
    (pp event buffer)))

(provide 'idris-events)

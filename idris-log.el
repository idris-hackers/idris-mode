;;; -*- lexical-binding: t -*-
;;; idris-log.el --- Logging of Idris

;; Copyright (C) 2013-2014 Hannes Mehnert and David Raymond Christiansen

;; Authors: Hannes Mehnert <hannes@mehnert.org>
;;          David Raymond Christiansen <drc@itu.dk>
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

(defvar idris-log-buffer-name (idris-buffer-name :log)
  "The name of the Idris log buffer.")

(defun idris-log-buffer ()
  "Return or create the log buffer."
  (or (get-buffer idris-log-buffer-name)
      (let ((buffer (get-buffer-create idris-log-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (set (make-local-variable 'outline-regexp) "^(")
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) "")
          (set (make-local-variable 'left-margin-width) 18)
          (setq buffer-read-only t))
        buffer)))

(defun idris-log (level message)
  "Record the fact that MESSAGE occured."
  ;; TODO: Different faces for different log levels
  (with-current-buffer (idris-log-buffer)
    (goto-char (point-max))
    (let* ((buffer-read-only nil)
           (time (substring (number-to-string (float-time)) 0 14))
           (meta-info (format "%14s %2s " time level))
           (meta (propertize " "
                             'display `((margin left-margin)
                                        ,meta-info))))
      (save-excursion
        (insert meta)
        (insert message)
        (insert "\n")))
    (goto-char (point-max))))

(defun idris-log-hook-function (event)
  (destructure-case event
    ((:log (level message) _target)
     (idris-log level message)
     t)
    (t nil)))

(provide 'idris-log)

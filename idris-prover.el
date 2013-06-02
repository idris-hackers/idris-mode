;;; idris-prover.el --- Prover mode for Idris

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

; consisting of three buffers:
; ------------------------------
; | proof obligations          |
; |----------------------------|
; | proof shell | proof script |
; ------------------------------

(defgroup idris-prover nil "Idris Prover" :prefix 'idris :group 'idris)

(defface idris-prover-processed-face
  '((t (:bold t)))
  "Face for Idris proof script which is already processed."
  :group 'idris-prover)

(defvar idris-prover-obligations-buffer-name (idris-buffer-name :proof-obligations)
  "The name of the Idris proof obligation buffer.")

(defvar idris-prover-shell-buffer-name (idris-buffer-name :proof-shell)
  "The name of the Idris proof shell buffer.")

(defvar idris-prover-script-buffer-name (idris-buffer-name :proof-script)
  "The name of the Idris proof script buffer.")

(defun idris-prover-obligations-buffer ()
  (or (get-buffer idris-prover-obligations-buffer-name)
      (let ((buffer (get-buffer-create idris-prover-obligations-buffer-name)))
        (with-current-buffer buffer
          (setq buffer-read-only t))
        buffer)))

(defun idris-prover-write-goals (goals)
  (interactive)
  (with-current-buffer (idris-prover-obligations-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert goals)
    (setq buffer-read-only t)))

(defvar-local idris-prover-script-processed nil
  "Marker for the processed part of proof script")

(defvar-local idris-prover-script-processing nil
  "Marker for the processing part of proof script")

(defvar-local idris-prover-script-processed-overlay nil
  "Overlay for processed proof script")

(defvar idris-prover-prove-step 0
  "Step counter of the proof")

(defun idris-prover-script-buffer ()
  (or (get-buffer idris-prover-script-buffer-name)
      (let ((buffer (get-buffer-create idris-prover-script-buffer-name)))
        (with-current-buffer buffer
          (setq buffer-read-only t)
          (set idris-prover-script-processed (make-marker))
          (set idris-prover-script-processing (make-marker)))
        buffer)))

(defun idris-prover-write-script (script i)
  (interactive)
  (with-current-buffer (idris-prover-script-buffer)
    ; the repl should be responsible to put content into this buffer..
    ; two cases: (well, three)
    ;  i < idris-prover-prove-step -> unmark stuff
    (cond ((< i idris-prover-prove-step)
           (goto-char idris-prover-script-processed)
           (forward-line -1)
           (end-of-line)
           (set-marker idris-prover-script-processed (point)))
    ;  i > idris-prover-prove-step -> mark stuff
          ((> i idris-prover-prove-step)
           (setq buffer-read-only nil)
           (goto-char idris-prover-script-processed)
           (let ((lelem (last script)))
             (insert-before-markers (concat "\n  " lelem ";")))
           (setq buffer-read-only t))
    ;  i = idris-prover-prove-step -> should not happen
          (t nil))
    (setq idris-prover-prove-step i)
    (unless (null idris-prover-script-processed-overlay)
      (delete-overlay idris-prover-script-processed-overlay))
    (let ((overlay (make-overlay 0 idris-prover-script-processed)))
      (overlay-put overlay 'read-only t)
      (overlay-put overlay 'face 'idris-prover-processed-face))))

(defun idris-prover-event-hook-function (event)
  (destructure-case event
    ((:start-proof-mode name target)
     (idris-repl-write-string "Start proof of ")
; reset buffers
     t)
    ((:end-proof-mode name target)
     (idris-repl-write-string "End proof of ")
; reset buffers
     t)
    ((:write-proof-state msg target)
     (destructuring-bind (script i) msg
       (idris-prover-write-script script i))
     t)
    ((:write-goal goal target)
     (idris-prover-write-goals goal)
     t)
    (t nil)))


(provide 'idris-prover)

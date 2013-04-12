;;; idris-repl.el --- Run an Idris interpreter using S-Expression communication protocol

;; Copyright (C) 2013  Hannes Mehnert

;; Author: Hannes Mehnert <hame@itu.dk>

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

(require 'inferior-idris)
(require 'idris-common-utils)

(defgroup idris-repl nil "Idris REPL" :prefix 'idris :group 'idris)

(defface idris-repl-prompt-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the prompt in the Idris REPL."
  :group 'idris-repl)

(defface idris-repl-output-face
  '((t (:inherit font-lock-string-face)))
  "Face for Idris output in the Idris REPL."
  :group 'idris-repl)

(defface idris-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the Idris REPL."
  :group 'idris-repl)

(defface idris-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the DIME REPL."
  :group 'idris-repl)

(defcustom idris-repl-history-file "~/.idris-history.eld"
  "File to save the persistent REPL history to."
  :type 'string
  :group 'idris-repl)

(defvar idris-prompt-string "Idris"
  "The prompt for the user")

(defvar idris-repl-buffer-name (idris-buffer-name :repl)
  "The name of the Idris REPL buffer.")

(make-variable-buffer-local
 (defvar idris-output-start nil
   "Marker for the start of the output for the evaluation."))

(make-variable-buffer-local
 (defvar idris-output-end nil
   "Marker for the end of the output. New output is inserted at this mark."))

(make-variable-buffer-local
 (defvar idris-prompt-start nil
   "Marker for the start of the Idris prompt."))

(make-variable-buffer-local
 (defvar idris-input-start nil
   "Marker for the start of user input for Idris."))

;; marker invariants maintained:
;; point-min <= output-start <= output-end <= prompt-start <= input-start <= point-max
(defun idris-mark-input-start ()
  (set-marker idris-input-start (point)))

(defun idris-mark-output-start ()
  (set-marker idris-output-start (point))
  (set-marker idris-output-end (point)))

; TODO: insert version number / protocol version / last changed date!?
(defun idris-repl-insert-banner ()
  "Insert Idris banner into buffer"
  (when (zerop (buffer-size))
    (let ((welcome "Welcome to Idris REPL!"))
      (insert welcome))))

(defun idris-repl-insert-prompt ()
  "Insert Idris prompt (before markers!) into buffer. Set point after prompt"
  (goto-char idris-input-start)
  (idris-save-marker idris-output-start
    (idris-save-marker idris-output-end
      (unless (bolp) (insert-before-markers "\n"))
      (let ((prompt-start (point))
            (prompt (format "%s> " idris-prompt-string)))
        (idris-propertize-region
         '(face idris-repl-prompt-face read-only t intangible t
                idris-repl-prompt t
                rear-nonsticky (idris-repl-prompt read-only face intangible))
         (insert-before-markers prompt))
        (set-marker idris-prompt-start prompt-start)
        prompt-start))))

(defun idris-repl-update-banner ()
  (idris-repl-insert-banner)
  (goto-char (point-max))
  (idris-mark-output-start)
  (idris-mark-input-start)
  (idris-repl-insert-prompt))

(defun idris-repl-buffer ()
  "Return or create the Idris REPL buffer."
  (or (get-buffer idris-repl-buffer-name)
      (let ((buffer (get-buffer-create idris-repl-buffer-name)))
        (with-current-buffer buffer
          (idris-repl-mode))
        buffer)))

(defvar idris-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'idris-repl-return)
    (define-key map [tab] 'idris-repl-complete)
    (define-key map [home] 'idris-repl-begin-of-prompt)
    (define-key map [?\C-a] 'idris-repl-begin-of-prompt)
    (define-key map [?\M-p] 'idris-repl-backward-history)
    (define-key map [C-up] 'idris-repl-backward-history)
    (define-key map [?\M-n] 'idris-repl-forward-history)
    (define-key map [C-down] 'idris-repl-forward-history)
    map)
  "Keymap used in Idris REPL mode.")

(defun idris-repl-mode ()
  "Major mode for interacting with Idris.
\\{idris-repl-mode-map}"
  (interactive)
  (setq major-mode 'idris-repl-mode)
  (setq mode-name "Idris-REPL")
  (dolist (markname '(idris-output-start
                      idris-output-end
                      idris-prompt-start
                      idris-input-start))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point)))
  (idris-repl-update-banner))

(defun idris-repl-current-input ()
  "Return the current input as string."
  (buffer-substring-no-properties idris-input-start (point-max)))

(defun idris-repl-return ()
  "Send command over to Idris"
  (let ((input (idris-repl-current-input)))
    (idris-eval-async `(:interpret ,input) idris-process
                      (lambda (result)
                        (destructuring-bind (output value) result
                          (push-mark)
                          (insert output value))))))

(defun idris-repl-complete ()
  "Completion of the current input")
;; need to do a sync req!

(defun idris-repl-begin-of-prompt ()
  "Got to the beginning of linke or the prompt."
  (interactive)
  (cond ((and (>= (point) idris-input-start)
              (idris-same-line-p (point) idris-input-start))
         (goto-char idris-input-start))
        (t (beginning-of-line 1))))

;;; history

(defun idris-repl-backward-history ()
  "Cycle backward through history.")

(defun idris-repl-forward-history ()
  "Cycle forward through history.")

(provide 'idris-repl)

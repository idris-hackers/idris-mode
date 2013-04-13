;;; idris-repl.el --- Run an Idris interpreter using S-Expression communication protocol

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

(require 'inferior-idris)
(require 'idris-common-utils)
(require 'idris-completion)

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
;; point-min <= idris-output-start <= idris-output-end <=
;;   idris-prompt-start <= idris-input-start <= point-max
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
        (set-marker idris-prompt-start prompt-start)))))

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

(defun idris-switch-to-output-buffer ()
  "Select the output buffer and scroll to bottom."
  (interactive)
  (pop-to-buffer (idris-repl-buffer))
  (goto-char (point-max)))

(defun idris-repl ()
  (interactive)
  (idris-run)
  (idris-switch-to-output-buffer))

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

;; Warning: Bug in idris-repl-mode: it forgets to call `run-mode-hooks'
;; also, keymap doesn't work :/
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

(defun idris-repl-return ()
  "Send command over to Idris"
  (goto-char (point-max))
  (let ((end (point)))
    (idris-repl-add-to-input-history (buffer-substring idris-input-start end))
    (let ((overlay (make-overlay idris-input-start end)))
      (overlay-put overlay 'read-only t)
      (overlay-put overlay 'face 'idris-repl-input-face)))
  (let ((input (idris-repl-current-input)))
    (goto-char (point-max))
    (idris-mark-input-start)
    (idris-mark-output-start)
    (idris-repl-eval-string input)))

(defun idris-repl-complete ()
  "Completion of the current input"
  (let* ((input (idris-repl-current-input))
         (result (idris-eval `(:repl-completions ,input))))
    (destructuring-bind (completions partial) result
      (if (null completions)
          (progn
            (idris-minibuffer-respecting-message "Can't find completions for \"%s\"" input)
            (ding)
            (idris-complete-restore-window-configuration))
        (insert-and-inherit (substring partial (length input)))
        (if (= (length completions) 1)
            (progn
              (idris-minibuffer-respecting-message "Sole completion")
              (idris-complete-restore-window-configuration))
          (idris-minibuffer-respecting-message "Completions, not unique")
          (idris-display-or-scroll-completions completions partial))))))

(defun idris-repl-begin-of-prompt ()
  "Got to the beginning of linke or the prompt."
  (interactive)
  (cond ((and (>= (point) idris-input-start)
              (idris-same-line-p (point) idris-input-start))
         (goto-char idris-input-start))
        (t (beginning-of-line 1))))

(defun idris-repl-current-input ()
  "Return the current input as string."
  (buffer-substring-no-properties idris-input-start (point-max)))

(defun idris-repl-eval-string (string)
  "Evaluate STRING on the superior Idris."
  (idris-rex ()
      ((list ':interpret string))
    ((:ok result)
     (destructuring-bind (output value) result
       (dolist (s output) (idris-repl-write-string s))
       (idris-repl-insert-result value)))
    ((:abort condition)
     (idris-repl-show-abort condition))))

(defun idris-repl-show-abort (condition)
  (with-current-buffer (idris-repl-buffer)
    (save-excursion
      (idris-save-marker idris-output-start
        (idris-save-marker idris-output-end
          (goto-char idris-output-end)
          (insert-before-markers (format "; Aborted: %s.\n" condition))
          (idris-repl-insert-prompt))))
    (idris-repl-show-maximum-output)))

(defun idris-repl-write-string (string)
  "Appends STRING to output"
  (with-current-buffer (idris-repl-buffer)
    (save-excursion
      (goto-char idris-output-end)
      (idris-save-marker idris-output-start
        (idris-propertize-region `(face idris-repl-output-face rear-nonsticky (face))
          (insert-before-markers string)
          (when (and (= (point) idris-prompt-start)
                     (not (bolp)))
            (insert-before-markers "\n")
            (set-marker idris-output-end (1- (point)))))))
    (idris-repl-show-maximum-output)))

(defun idris-repl-insert-result (string)
  "Inserts STRING and marks it as evaluation result"
  (with-current-buffer (idris-repl-buffer)
    (save-excursion
      (idris-save-marker idris-output-start
        (idris-save-marker idris-output-end
          (goto-char idris-input-start)
          (when (not (bolp)) (insert-before-markers "\n"))
          (idris-propertize-region `(face idris-repl-result-face rear-nonsticky (face))
            (insert-before-markers string)))))
    (idris-repl-insert-prompt)
    (idris-repl-show-maximum-output)))

(defun idris-repl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (if (eq (window-buffer) (current-buffer))
                   (selected-window)
                 (get-buffer-window (current-buffer) t))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max))
          (recenter -1))))))


;;; history

(defun idris-repl-add-to-input-history (input)
  "Adds input to history.")

(defun idris-repl-backward-history ()
  "Cycle backward through history.")

(defun idris-repl-forward-history ()
  "Cycle forward through history.")

(provide 'idris-repl)

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
  "Face for the result of an evaluation in the Idris REPL."
  :group 'idris-repl)

(defcustom idris-repl-history-file "~/.idris-history.eld"
  "File to save the persistent REPL history to."
  :type 'string
  :group 'idris-repl)

(defcustom idris-repl-history-size 200
  "*Maximum number of lines for persistent REPL history."
  :type 'integer
  :group 'idris-repl)

(defcustom idris-repl-history-file-coding-system
  'utf-8-unix
  "*The coding system for the history file."
  :type 'symbol
  :group 'idris-repl)

(defvar idris-prompt-string "Idris"
  "The prompt for the user")

(defvar idris-repl-buffer-name (idris-buffer-name :repl)
  "The name of the Idris REPL buffer.")

(defvar-local idris-output-start nil
  "Marker for the start of the output for the evaluation.")

(defvar-local idris-output-end nil
  "Marker for the end of the output. New output is inserted at this mark.")

(defvar-local idris-prompt-start nil
  "Marker for the start of the Idris prompt.")

(defvar-local idris-input-start nil
  "Marker for the start of user input for Idris.")

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
  (when (not (null idris-input-start))
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
          (set-marker idris-prompt-start prompt-start))))))

(defun idris-repl-update-prompt (new-prompt)
  "Update prompt to NEW-PROMPT"
  (unless (equal idris-prompt-string new-prompt)
    (setq idris-prompt-string new-prompt)
    (with-current-buffer (idris-repl-buffer)
      (idris-repl-insert-prompt))))

(defun idris-repl-buffer ()
  "Return or create the Idris REPL buffer."
  (or (get-buffer idris-repl-buffer-name)
      (let ((buffer (get-buffer-create idris-repl-buffer-name)))
        (with-current-buffer buffer
          (idris-repl-mode)
          (idris-repl-buffer-init))
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

(easy-menu-define idris-repl-mode-menu idris-repl-mode-map
  "Menu for the Idris REPL mode"
  `("Idris REPL"
    ["Customize idris-mode" (customize-group 'idris) t]
    ["Quit inferior idris process" idris-quit t]
    ))

(define-derived-mode idris-repl-mode fundamental-mode "Idris-REPL"
  "Major mode for interacting with Idris.
    \\{idris-repl-mode-map}
Invokes `idris-repl-mode-hook'."
  ;syntax-table?
  :group 'idris-repl
  (set (make-local-variable 'indent-tabs-mode) nil)
  (add-hook 'idris-event-hooks 'idris-repl-event-hook-function)
  (add-hook 'kill-buffer-hook 'idris-repl-remove-event-hook-function nil t)
  (when idris-repl-history-file
    (idris-repl-safe-load-history)
    (add-hook 'kill-buffer-hook
              'idris-repl-safe-save-history nil t))
  (add-hook 'kill-emacs-hook 'idris-repl-save-all-histories))

(defun idris-repl-remove-event-hook-function ()
  (setq idris-prompt-string "Idris")
  (remove-hook 'idris-event-hooks 'idris-repl-event-hook-function))

(defun idris-repl-event-hook-function (event)
  (destructure-case event
    ((:write-string output target)
     ; target currently unused
     (idris-repl-write-string output)
     t)
    ((:set-prompt prompt target)
     (idris-repl-update-prompt prompt)
     t)
    (t nil)))

(defun idris-repl-update-banner ()
  (idris-repl-insert-banner)
  (goto-char (point-max))
  (idris-mark-output-start)
  (idris-mark-input-start)
  (idris-repl-insert-prompt))

(defun idris-repl-buffer-init ()
  (dolist (markname '(idris-output-start
                      idris-output-end
                      idris-prompt-start
                      idris-input-start))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point)))
  (idris-repl-update-banner))

(defun idris-repl-return ()
  "Send command over to Idris"
  (interactive)
  (goto-char (point-max))
  (let ((end (point)))
    (idris-repl-add-to-input-history (buffer-substring idris-input-start end))
    (let ((overlay (make-overlay idris-input-start end)))
      (overlay-put overlay 'face 'idris-repl-input-face)))
  (let ((input (idris-repl-current-input)))
    (goto-char (point-max))
    (insert "\n")
    (idris-mark-input-start)
    (idris-mark-output-start)
    (idris-repl-eval-string input)))

(defun idris-repl-complete ()
  "Completion of the current input"
  (interactive)
  (let* ((input (idris-repl-current-input))
         (result (idris-eval `(:repl-completions ,input))))
    (destructuring-bind (completions partial) result
      (if (null completions)
          (progn
            (idris-minibuffer-respecting-message "Can't find completions for \"%s\"" input)
            (ding)
            (idris-complete-restore-window-configuration))
          (if (= (length completions) 1)
              (progn
                (insert-and-inherit (substring (concat partial (car completions)) (length input)))
                (idris-minibuffer-respecting-message "Sole completion")
                (idris-complete-restore-window-configuration))
            (let* ((pp (substring input (length partial)))
                   (mypartial (find-common-prefix pp completions)))
              (insert-and-inherit (substring (concat partial mypartial) (length input)))
              (idris-minibuffer-respecting-message "Completions, not unique")
              (idris-display-or-scroll-completions completions partial mypartial)))))))

(defun find-common-prefix (input slist)
  "Finds longest common prefix of all strings in list."
  (let ((first (car slist))
        (ilen (length input)))
    (if (> (length first) ilen)
        (progn
          (let ((next (substring first 0 (1+ ilen))))
            (if (every (lambda (p) (string-prefix-p next p)) slist)
                (find-common-prefix next slist)
              input)))
      input)))

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
  "Evaluate STRING on the inferior Idris."
  (idris-rex ()
      ((list ':interpret string))
    ((:ok result)
     (idris-repl-insert-result result))
    ((:error condition)
     (idris-repl-show-abort condition))))

(defun idris-repl-show-abort (condition)
  (with-current-buffer (idris-repl-buffer)
    (save-excursion
      (idris-save-marker idris-output-start
        (idris-save-marker idris-output-end
          (goto-char idris-output-end)
          (insert-before-markers (format "; Error: %s.\n" condition))
          (idris-repl-insert-prompt))))
    (idris-repl-show-maximum-output)))

(defun idris-repl-write-string (string)
  "Appends STRING to output"
  (with-current-buffer (idris-repl-buffer)
    (save-excursion
      (goto-char idris-output-end)
      (idris-save-marker idris-output-start
        (idris-propertize-region `(face idris-repl-output-face rear-nonsticky (face))
          (when (not (bolp)) (insert-before-markers "\n"))
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

(defvar-local idris-repl-input-history '()
  "History list of strings entered into the REPL buffer.")

(defun idris-repl-add-to-input-history (string)
  "Adds input to history."
  (unless (equal string "")
    (setq idris-repl-input-history
          (remove string idris-repl-input-history)))
  (unless (equal string (car idris-repl-input-history))
      (push string idris-repl-input-history)))

(defvar-local idris-repl-input-history-position -1
  "Newer items have smaller indices.")

(defun idris-repl-delete-current-input ()
  "Delete all text from the prompt."
  (interactive)
  (delete-region idris-input-start (point-max)))

(defun idris-repl-replace-input (string)
  (idris-repl-delete-current-input)
  (insert-and-inherit string))

(defun idris-repl-history-replace (direction)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list)."
  (let* ((min-pos -1)
         (max-pos (length idris-repl-input-history))
         (prefix (idris-repl-history-prefix))
         (pos0 (if (idris-repl-history-search-in-progress-p)
                   idris-repl-input-history-position
                 min-pos))
         (pos (idris-repl-position-in-history pos0 direction prefix))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (idris-repl-replace-input (nth pos idris-repl-input-history))
           (setq msg (format "History item: %d" pos)))
          (t
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (message "%s (prefix is: %s)" msg prefix)
    (setq idris-repl-input-history-position pos)
    (setq this-command 'idris-repl-history-replace)))

(defvar-local idris-repl-history-prefix-data ""
  "Current history prefix.")

(defun idris-repl-history-prefix ()
  "Return the prefix we want to look for in the history."
  (if (idris-repl-history-search-in-progress-p)
      idris-repl-history-prefix-data
    (setq idris-repl-history-prefix-data (idris-repl-current-input))
    idris-repl-history-prefix-data))

(defun idris-repl-history-search-in-progress-p ()
  (eq last-command 'idris-repl-history-replace))

(defun idris-repl-position-in-history (start-pos direction prefix)
  "Return the position of the history item matching the PREFIX.
Return -1 resp. the length of the history if no item matches."
  ;; Loop through the history list looking for a matching line
  (let* ((step (ecase direction
                 (forward -1)
                 (backward 1)))
         (history idris-repl-input-history)
         (len (length history)))
    (loop for pos = (+ start-pos step) then (+ pos step)
          if (< pos 0) return -1
          if (<= len pos) return len
          for history-item = (nth pos history)
          if (string-prefix-p prefix history-item)
          return pos)))

(defun idris-repl-backward-history ()
  "Cycle backward through history."
  (interactive)
  (idris-repl-history-replace 'backward))

(defun idris-repl-forward-history ()
  "Cycle forward through history."
  (interactive)
  (idris-repl-history-replace 'forward))


;; persistent history
(defun idris-repl-save-all-histories ()
  "Save the history in each repl buffer."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'idris-repl-mode)
        (idris-repl-safe-save-history)))))

(defun idris-repl-safe-save-history ()
  (idris-repl-call-with-handler
   #'idris-repl-save-history
   "%S while saving the history. Continue? "))

(defun idris-repl-safe-load-history ()
  (idris-repl-call-with-handler
   #'idris-repl-load-history
   "%S while loading the history. Continue? "))

(defun idris-repl-call-with-handler (fun query)
  "Call FUN in the context of an error handler.
The handler will use qeuery to ask the use if the error should be ingored."
  (condition-case err
      (funcall fun)
    (error
     (if (y-or-n-p (format query (error-message-string err)))
         nil
       (signal (car err) (cdr err))))))

(defun idris-repl-load-history (&optional filename)
  "Set the current Idris REPL history.
It can be read either from FILENAME or `idris-repl-history-file' or
from a user defined filename."
  (interactive (list (idris-repl-read-history-filename)))
  (let ((file (or filename idris-repl-history-file)))
    (setq idris-repl-input-history (idris-repl-read-history file t))))

(defun idris-repl-read-history (&optional filename noerrer)
  "Read and return the history from FILENAME.
The default value for FILENAME is `idris-repl-history-file'.
If NOERROR is true return and the file doesn't exits return nil."
  (let ((file (or filename idris-repl-history-file)))
    (cond ((not (file-readable-p file)) '())
          (t (with-temp-buffer
               (insert-file-contents file)
               (read (current-buffer)))))))

(defun idris-repl-save-history (&optional filename history)
  "Simply save the current Idris REPL history to a file.
When Idris is setup to always load the old history and one uses only
one instance of idris all the time, there is no need to merge the
files and this function is sufficient."
  (interactive (list (idris-repl-read-history-filename)))
  (let ((file (or filename idris-repl-history-file))
        (hist (or history idris-repl-input-history)))
    (unless (file-writable-p file)
      (error (format "History file not writable: %s" file)))
    (let ((hist (subseq hist 0 (min (length hist) idris-repl-history-size))))
      (with-temp-file file
        (let ((cs idris-repl-history-file-coding-system)
              (print-length nil) (print-level nil))
          (setq buffer-file-coding-system cs)
          (insert (format ";; -*- coding: %s -*-\n" cs))
          (insert ";; History for Idris REPL. Automatically written.\n"
                  ";; Edit only if you know what you're doing\n")
          (prin1 (mapcar #'substring-no-properties hist) (current-buffer)))))))

(provide 'idris-repl)

;;; idris-prover.el --- Prover mode for Idris -*- lexical-binding: t -*-

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
(require 'idris-warnings)

; consisting of three buffers:
; ------------------------------
; | proof obligations          |
; |----------------------------|
; | proof shell | proof script |
; ------------------------------

(defgroup idris-prover nil "Idris Prover" :prefix 'idris :group 'idris)

(defface idris-prover-processed-face
  '((t (:background "spring green")))
  "Face for Idris proof script which is already processed."
  :group 'idris-prover)

(defface idris-prover-processing-face
  '((t (:background "gold")))
  "Face for Idris proof script which is currently processing."
  :group 'idris-prover)

(defcustom idris-prover-restore-window-configuration t
  "When non-nil, restore the window configuration after exiting
the prover."
  :type 'boolean
  :group 'idris-prover)

(defvar idris-prover-obligations-buffer-name (idris-buffer-name :proof-obligations)
  "The name of the Idris proof obligation buffer.")

(defvar idris-prover-shell-buffer-name (idris-buffer-name :proof-shell)
  "The name of the Idris proof shell buffer.")

(defvar idris-prover-script-buffer-name (idris-buffer-name :proof-script)
  "The name of the Idris proof script buffer.")

(defvar idris-prover-currently-proving nil
  "The metavariable that Idris has open in the interactive
prover, or nil if Idris is not proving anything.")

(defun idris-prover-obligations-buffer ()
  (or (get-buffer idris-prover-obligations-buffer-name)
      (let ((buffer (get-buffer-create idris-prover-obligations-buffer-name)))
        (with-current-buffer buffer
          (setq buffer-read-only t))
        buffer)))

(defun idris-prover-show-obligations ()
  (display-buffer (idris-prover-obligations-buffer)
     '(display-buffer-pop-up-window . nil)))

(defun idris-prover-write-goals (goals)
  (with-current-buffer (idris-prover-obligations-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert goals)))
  (idris-prover-show-obligations))

(defvar idris-prover-saved-window-configuration nil
  "The saved window configuration from before running the prover.")

(defvar-local idris-prover-script-processed nil
  "Marker for the processed part of proof script")

(defvar-local idris-prover-script-processed-overlay nil
  "Overlay for processed proof script")

(defvar-local idris-prover-script-processing nil
  "Marker for the processing part of proof script")

(defvar-local idris-prover-script-processing-overlay nil
  "Overlay for processing proof script")

(defvar-local idris-prover-script-warning-overlay nil
  "Overlay for warning in proof script")

; invariant: point-min <= idris-prover-script-processed <= idris-prover-script-processing <= point-max

(defvar-local idris-prover-prove-step 0
  "Step counter of the proof")

(defvar idris-prover-script-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'idris-prover-script-ret)
    (define-key map (kbd "M-n") 'idris-prover-script-forward)
    (define-key map (kbd "M-p") 'idris-prover-script-backward)
    ;; Using (kbd "<TAB>") in place of "\t" makes emacs angry, and suggests
    ;; using the latter form.
    (define-key map "\t" 'idris-prover-script-complete)
    map)
  "Keymap used in Idris proof script mode.")

(defun idris-prover-find-tactic (start-pos)
  "Use some layout heuristics to find the tactic beginning at
START-POS, returning a pair consisting of the start and end
positions of the tactic. Tactics are required to begin at the
left margin."
  (let (tactic-start tactic-end)
    (save-excursion
      (goto-char start-pos)

      ;; Ensure that we're at the next line beginning
      (beginning-of-line)
      (unless (= (point) start-pos)
        (forward-line))

      ;; Go forward until the current line begins a tactic
      (while (and (not (eobp)) (not (looking-at-p "[a-zA-Z]")))
        (forward-line))

      (if (eobp) ;; if at end of buffer, no tactic to be found!
          nil
        (setq tactic-start (point))

        ;; Go forward until end of buffer or non-blank line at left margin
        (forward-line)
        (while (and (not (eobp)) (not (looking-at-p "[a-zA-Z]")))
          (forward-line))

        ;; Go backward until a non-whitespace char is found - it is the end of
        ;; the tactic
        (backward-char)
        (while (looking-at-p "\\s-\\|$") (backward-char))
        (forward-char)
        (setq tactic-end (point))

        (cons tactic-start tactic-end)))))

(defun idris-prover-script-backward ()
  "Backward one piece of proof script"
  (interactive)
  (idris-rex ()
      ((list ':interpret "undo"))
    ((:ok result)
     t)
    ((:error condition)
     ;; put error overlay
     (message (concat "fail: " condition))
     t)))

(defun idris-prover-script-forward ()
  "Forward one piece of proof script."
  (interactive)
  (when (eobp) (newline)) ;; There must be a newline at the end
  (when idris-prover-script-warning-overlay
    (delete-overlay idris-prover-script-warning-overlay)
    (setq idris-prover-script-warning-overlay nil))
  (goto-char (+ 1 idris-prover-script-processed))
  (let ((next-tactic (idris-prover-find-tactic
                      idris-prover-script-processed)))
    (if (null next-tactic)
        (error "At the end of the proof script")
      (let* ((tactic-start (car next-tactic))
             (tactic-end (cdr next-tactic))
             (tactic-text (buffer-substring-no-properties tactic-start
                                                          tactic-end)))
        (set-marker idris-prover-script-processed tactic-start)
        (set-marker idris-prover-script-processing tactic-end)
        (let ((overlay (make-overlay idris-prover-script-processed
                                     idris-prover-script-processing)))
          (overlay-put overlay 'face 'idris-prover-processing-face)
          (setq idris-prover-script-processing-overlay overlay))
        (let ((tactic-cmd (replace-regexp-in-string
                           "\\`[ \t\n]*" ""
                           (replace-regexp-in-string "" " " tactic-text))))
          (idris-rex () ((list ':interpret tactic-cmd))
            ((:ok result)
             (with-current-buffer (idris-prover-script-buffer)
               (when idris-prover-script-processing-overlay
                 (delete-overlay idris-prover-script-processing-overlay)
                 (setq idris-prover-script-processing-overlay nil))
               ;; Delete the region because it will be written with the proof
               ;; state.
               (delete-region idris-prover-script-processed
                              idris-prover-script-processing)))
            ((:error condition)
             (with-current-buffer (idris-prover-script-buffer)
               (when idris-prover-script-processing-overlay
                 (delete-overlay idris-prover-script-processing-overlay)
                 (setq idris-prover-script-processing-overlay nil))
               (setq idris-prover-script-warning-overlay (idris-warning-create-overlay idris-prover-script-processed idris-prover-script-processing condition)))
                    ; put error overlay
             (message (concat "fail: " condition))
             t)))))))

(defun idris-prover-script-ret ()
  "Insert a newline at the end of buffer, even if it's read-only."
  (interactive)
  (if (equal (point) (marker-position idris-prover-script-processed))
      (let ((inhibit-read-only t)) (insert "\n"))
    (newline)))

(defun idris-prover-script-complete ()
  "Completion of the partial input."
  (interactive)
  (goto-char idris-prover-script-processed)
  (let* ((input (buffer-substring-no-properties (point) (point-max)))
         (inputp (replace-regexp-in-string "[ \t\n]*\\'" "" input))
         (inputr (replace-regexp-in-string "\\`[ \t\n]*" "" inputp))
         (result (idris-eval `(:repl-completions ,inputr))))
    (destructuring-bind (completions partial) result
      (if (null completions)
          (progn
            (idris-minibuffer-respecting-message "Can't find completions for \"%s\"" input)
            (ding)
            (idris-complete-restore-window-configuration))
        (if (= (length completions) 1)
            (progn
              (goto-char (+ idris-prover-script-processed (length inputp)))
              (insert-and-inherit (substring (concat partial (car completions)) (length inputr)))
              (idris-minibuffer-respecting-message "Sole completion")
              (idris-complete-restore-window-configuration))
          (let* ((pp (substring input (length partial)))
                 (mypartial (find-common-prefix pp completions)))
            (insert-and-inherit (substring (concat partial mypartial) (length input)))
            (idris-minibuffer-respecting-message "Completions, not unique")
            (idris-display-or-scroll-completions completions partial mypartial)))))))

(define-derived-mode idris-prover-script-mode prog-mode "Idris-Proof-Script"
  "Major mode for interacting with Idris proof script.
    \\{idris-prover-script-mode-map}
Invokes `idris-prover-script-mode-hook'."
  :group 'idris-prover
  (set (make-local-variable 'indent-tabs-mode) nil))

(defun idris-prover-script-buffer ()
  (or (get-buffer idris-prover-script-buffer-name)
      (let ((buffer (get-buffer-create idris-prover-script-buffer-name)))
        (with-current-buffer buffer
          (idris-prover-script-mode)
          (setq idris-prover-script-processing (make-marker))
          (setq idris-prover-script-processed (make-marker))
          (set-marker idris-prover-script-processing (point))
          (set-marker idris-prover-script-processed (point)))
        buffer)))

(defun idris-prover-reset-prover-script-buffer ()
  "Erase or initialize a proof script buffer, resetting all the
special prover state."
  (with-current-buffer (idris-prover-script-buffer)
    (when idris-prover-script-processed-overlay
      (delete-overlay idris-prover-script-processed-overlay)
      (setq idris-prover-script-processed-overlay nil))
    (when idris-prover-script-processing-overlay
      (delete-overlay idris-prover-script-processing-overlay)
      (setq idris-prover-script-processing-overlay nil))
    (setq idris-prover-prove-step 0)
    (erase-buffer)
    (unless idris-prover-script-processing
      (setq idris-prover-script-processing (make-marker)))
    (unless idris-prover-script-processed
      (setq idris-prover-script-processed (make-marker)))
    (set-marker idris-prover-script-processing (point))
    (set-marker idris-prover-script-processed (point))))

(defun idris-prover-write-script (script count)
  "Put the proof state recieved from Idris into the proof script buffer.
SCRIPT is the list of tactics in the proof state, and COUNT is
the length reported by Idris."
  (interactive)
  (with-current-buffer (idris-prover-script-buffer)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point-max) 'read-only nil))
    (cond ((< count idris-prover-prove-step)
           ; this is actually the (count - 1) == Idris-prover-prove-step case!
           ; in other words, we are undoing the final step.
           ; can the other case(s) happen??
           (goto-char idris-prover-script-processed)
           (forward-line -1)
           (end-of-line)
           (set-marker idris-prover-script-processed (point)))
          ((> count idris-prover-prove-step)
           ;; Here we are inserting a newly-checked proof step.
           (goto-char idris-prover-script-processed)
           (while (< idris-prover-prove-step count)
             (let ((lelem (nth idris-prover-prove-step script)))
               (insert-before-markers lelem))
             (newline)
             (setq idris-prover-prove-step (1+ idris-prover-prove-step))))
          (t nil))
    (setq idris-prover-prove-step count)
    (when (eql (marker-position idris-prover-script-processed) (point-max))
      (goto-char idris-prover-script-processed)
      (insert "\n"))
    (unless (null idris-prover-script-processed-overlay)
      (delete-overlay idris-prover-script-processed-overlay))
    (let ((overlay (make-overlay 0 idris-prover-script-processed)))
      (overlay-put overlay 'face 'idris-prover-processed-face)
      (setq idris-prover-script-processed-overlay overlay))
    (let ((inhibit-read-only t))
      (put-text-property (point-min) idris-prover-script-processed 'read-only t))
    (goto-char (1+ (marker-position idris-prover-script-processed)))))

(defun idris-prover-abandon ()
  "Abandon an in-progress proof."
  (interactive)
  (if idris-prover-currently-proving
      (idris-eval (list :interpret "abandon") t)
    (error "No proof in progress")))

(defun idris-prover-end ()
  "Get rid of left over buffers from proof mode and unset global state related to the prover."
  (interactive)
  (setq idris-prover-currently-proving nil)
  (let ((obligations (idris-prover-obligations-buffer))
        (script (idris-prover-script-buffer)))
    (when obligations
      (delete-windows-on obligations)
      (kill-buffer obligations))
    (when script (kill-buffer script)))
  (when (and idris-prover-restore-window-configuration
             (window-configuration-p
              idris-prover-saved-window-configuration))
    (set-window-configuration idris-prover-saved-window-configuration))
  (setq idris-prover-saved-window-configuration nil))

(defun idris-prover-event-hook-function (event)
  "Process an EVENT returned from Idris when the prover is running."
  (destructure-case event
    ((:start-proof-mode name _target)
     (setq idris-prover-currently-proving name)
     (setq idris-prover-saved-window-configuration
           (current-window-configuration))
     (idris-prover-reset-prover-script-buffer)
     (idris-repl-write-string (format "Start proof of %s" name))
     (let* ((obligations-window (idris-prover-show-obligations))
            (script-window (split-window obligations-window)))
       (set-window-buffer script-window (idris-prover-script-buffer)))
     t)
    ((:end-proof-mode msg _target)
     (let ((name (car msg))
           (proof (cadr msg)))
       (idris-perhaps-insert-proof-script proof)
       (idris-prover-end)
       (idris-repl-write-string (concat "End proof of " name))
       (run-hooks 'idris-prover-success-hook))
     t)
    ((:write-proof-state msg _target)
     (destructuring-bind (script count) msg
       (idris-prover-write-script script count))
     t)
    ((:write-goal goal _target)
     (idris-prover-write-goals goal)
     t)
    ((:abandon-proof _msg _target)
     (idris-prover-end)
     (idris-repl-write-string "Abandoned proof")
     t)
    (t nil)))

(defcustom idris-prover-success-hook '(idris-list-metavariables-on-load)
  "Functions to call when completing a proof"
  :type 'hook
  :options '(idris-list-metavariables-on-load)
  :group 'idris-prover)

(defun idris-perhaps-insert-proof-script (proof)
  "Prompt the user as to whether PROOF should be inserted into the buffer."
  (save-window-excursion
    (pop-to-buffer idris-currently-loaded-buffer)
    (delete-other-windows)
    (let ((proof-buffer (get-buffer-create "*idris-finished-proof*")))
      (unwind-protect
          (progn
            (pop-to-buffer proof-buffer)
            (insert proof)
            (if (y-or-n-p "Keep this proof script?")
                (idris-insert-proof-script idris-currently-loaded-buffer proof)
              (kill-new proof)
              (message "Proof saved to kill ring")))
        (kill-buffer proof-buffer)))))

(defconst idris-proof-script-insertion-marker "---------- Proofs ----------"
  "Look for this marker to insert proofs. Should agree with the
  one in the Idris compiler source.")

(defun idris-insert-proof-script (buffer proof)
  "Insert PROOF into BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward idris-proof-script-insertion-marker nil t)
        (replace-regexp "\\(\\s-*\n\\)*\\'"
                        (concat "\n\n" idris-proof-script-insertion-marker "\n")))
      (newline)
      (insert proof))))

(provide 'idris-prover)

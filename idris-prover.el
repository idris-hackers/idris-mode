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

(defun idris-prover-show-obligations ()
  (display-buffer (idris-prover-obligations-buffer)
     '(display-buffer-pop-up-window . nil)))

(defun idris-prover-write-goals (goals)
  (with-current-buffer (idris-prover-obligations-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert goals)))
  (idris-prover-show-obligations))


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
    (define-key map (kbd "C-n") 'idris-prover-script-forward)
    (define-key map (kbd "C-p") 'idris-prover-script-backward)
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
     (message (concat "success: " result)))
    ((:error condition)
     ;; put error overlay
     (message (concat "fail: " condition)))))

(defun idris-prover-script-forward ()
  "Forward one piece of proof script"
  (interactive)
  (when idris-prover-script-warning-overlay
    (delete-overlay idris-prover-script-warning-overlay)
    (setq idris-prover-script-warning-overlay nil))
  (goto-char (+ 1 idris-prover-script-processed))
  (let ((next-tactic (idris-prover-find-tactic
                      (1+ idris-prover-script-processed))))
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
        (let ((tactic-cmd (replace-regexp-in-string "\\`[ \t\n]*" ""
                                                    (replace-regexp-in-string "" " " tactic-text))))
          (idris-rex () ((list ':interpret tactic-cmd))
            ((:ok result)
             (with-current-buffer (idris-prover-script-buffer)
               (when idris-prover-script-processing-overlay
                 (delete-overlay idris-prover-script-processing-overlay)
                 (setq idris-prover-script-processing-overlay nil))
               (delete-region idris-prover-script-processed idris-prover-script-processing))
             (message (concat "success: " result)))
            ((:error condition)
             (with-current-buffer (idris-prover-script-buffer)
               (when idris-prover-script-processing-overlay
                 (delete-overlay idris-prover-script-processing-overlay)
                 (setq idris-prover-script-processing-overlay nil))
               (setq idris-prover-script-warning-overlay (idris-warning-create-overlay idris-prover-script-processed idris-prover-script-processing condition)))
                    ; put error overlay
             (message (concat "fail: " condition)))))))))

(defun idris-prover-script-complete ()
  "Completion of the partial input"
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
    (set-marker idris-prover-script-processed (point))
    (insert "\n")))

(defun idris-prover-write-script (script i)
  (interactive)
  (with-current-buffer (idris-prover-script-buffer)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point-max) 'read-only nil))
    (cond ((< i idris-prover-prove-step)
           ; this is actually the (i - 1) == Idris-prover-prove-step case!
           ; can the other case(s) happen??
           (goto-char idris-prover-script-processed)
           (forward-line -1)
           (end-of-line)
           (set-marker idris-prover-script-processed (point)))
          ((> i idris-prover-prove-step)
           (goto-char idris-prover-script-processed)
           (while (< idris-prover-prove-step i)
             (let ((lelem (nth idris-prover-prove-step script)))
               (insert-before-markers (concat "\n" lelem)))
             (setq idris-prover-prove-step (1+ idris-prover-prove-step))))
          (t nil))
    (setq idris-prover-prove-step i)
    (when (eql (marker-position idris-prover-script-processed) (point-max))
      (goto-char idris-prover-script-processed)
      (insert "\n"))
    (unless (null idris-prover-script-processed-overlay)
      (delete-overlay idris-prover-script-processed-overlay))
    (let ((overlay (make-overlay 0 idris-prover-script-processed)))
      (overlay-put overlay 'face 'idris-prover-processed-face)
      (setq idris-prover-script-processed-overlay overlay))
    (let ((inhibit-read-only t))
      (put-text-property (point-min) idris-prover-script-processed 'read-only t))))

(defun idris-prover-event-hook-function (event)
  (destructure-case event
    ((:start-proof-mode _name _target)
     (idris-prover-reset-prover-script-buffer)
     (idris-repl-write-string "Start proof of ")
     (let* ((obligations-window (idris-prover-show-obligations))
            (script-window (split-window obligations-window)))
       (set-window-buffer script-window (idris-prover-script-buffer)))
     t)
    ((:end-proof-mode name _target)
     (idris-repl-write-string (concat "End proof of " name))
     t)
    ((:write-proof-state msg _target)
     (destructuring-bind (script i) msg
       (idris-prover-write-script script i))
     t)
    ((:write-goal goal _target)
     (idris-prover-write-goals goal)
     t)
    (t nil)))


(provide 'idris-prover)

;;; inferior-idris-mode.el --- Run an Idris interpreter inside of comint

;; Copyright (C) 2013  David Raymond Christiansen and Hannes Mehnert

;; Author: David Raymond Christiansen <drc@itu.dk>, Hannes Mehnert <hame@itu.dk>
;; Keywords: languages

;; License:
;; Inspiration and some methods have been taken from haskell-mode (https://github.com/haskell/haskell-mode/)
;; Other inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
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

;;; Code:
(require 'comint)
(require 'idris-syntax)

(defconst idris-prompt-regexp "[^>]+> *")

(defvar inferior-idris-buffer nil
  "The buffer in which the inferior process is running.")

(define-derived-mode
    inferior-idris-mode comint-mode "Idris>"
    "Run an Idris interpreter."
    :syntax-table idris-syntax-table
    (setq comint-use-prompt-regexp t)
    (setq comint-prompt-regexp (concat "^" idris-prompt-regexp))
    (add-hook 'comint-output-filter-functions 'inferior-idris-spot-prompt nil t)
    (set (make-local-variable 'font-lock-defaults)
         idris-font-lock-defaults))

(defun inferior-idris-start-process ()
  "Start an inferior Idris process"
  (interactive)
  (setq inferior-idris-buffer
        (make-comint "inferior-idris" idris-interpreter-path))
  (with-current-buffer inferior-idris-buffer
    (inferior-idris-mode)
    (setq inferior-idris-working-directory nil)
    (setq inferior-idris-loaded-file nil)))

(defun inferior-idris-process ()
  "Return the Idris process or start it"
  (or (if (buffer-live-p inferior-idris-buffer)
          (get-buffer-process inferior-idris-buffer))
      (progn
        (call-interactively 'inferior-idris-start-process)
        (inferior-idris-process))))

(defun inferior-idris-send-command (proc str)
  (setq str (concat str "\n"))
  (with-current-buffer (process-buffer proc)
    (inferior-idris-wait-for-prompt proc)
    (goto-char (process-mark proc))
    (insert-before-markers str)
    (move-marker comint-last-input-end (point))
    (setq inferior-idris-seen-prompt nil)
    (comint-send-string proc str)))

(defvar inferior-idris-seen-prompt nil)
(make-variable-buffer-local 'inferior-idris-seen-prompt)

(defvar inferior-idris-look-for-warnings nil)
(defvar inferior-idris-warnings '())

(defun inferior-idris-spot-prompt (string)
  (if inferior-idris-look-for-warnings
      (mapc #'(lambda (s)
                (if (string-prefix-p (concat inferior-idris-loaded-file ":") s)
                    (push (split-string s ":") inferior-idris-warnings)))
            (split-string string "\n")))
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (save-excursion
        (goto-char (process-mark proc))
        (if (re-search-backward
             (concat "^" idris-prompt-regexp)
             (line-beginning-position) t)
            (progn (setq inferior-idris-seen-prompt t)
                   (if inferior-idris-look-for-warnings
                       (idris-higlight-warnings inferior-idris-warnings))))))))

(defun inferior-idris-wait-for-prompt (proc &optional timeout)
  "Wait until PROC sends us a prompt.
The process PROC should be associated to a comint buffer."
  (with-current-buffer (process-buffer proc)
    (while (progn
             (goto-char comint-last-input-end)
             (not (or inferior-idris-seen-prompt
                      (setq inferior-idris-seen-prompt
                            (re-search-forward
                             (concat "^" idris-prompt-regexp) nil t))
                      (not (accept-process-output proc timeout))))))
    (unless inferior-idris-seen-prompt
      (error "Can't find the prompt"))))

(defvar inferior-idris-working-directory nil
  "The current working directory of Idris")
(make-variable-buffer-local 'inferior-idris-working-directory)

(defvar inferior-idris-loaded-file nil
  "The currently loaded file of Idris")
(make-variable-buffer-local 'inferior-idris-loaded-file)

(defvar inferior-idris-loaded-buffer nil
  "The currently loaded buffer of Idris")

(defun inferior-idris-load-file ()
  "Pass the current buffer's file to the inferior Idris process."
  (interactive)
  (setq inferior-idris-loaded-buffer (current-buffer))
  (save-buffer)
  (let* ((file buffer-file-name)
         (relfile (file-relative-name file))
         (filedir (file-name-directory file))
         (proc (inferior-idris-process)))
    (switch-to-buffer-other-window inferior-idris-buffer)
    (if file
        (with-current-buffer (process-buffer proc)
          (unless (equal inferior-idris-working-directory filedir)
            (setq inferior-idris-working-directory filedir)
            (inferior-idris-send-command proc (concat ":cd " inferior-idris-working-directory)))
          (setq inferior-idris-look-for-warnings t)
          (setq inferior-idris-warnings '())
          (if (equal relfile inferior-idris-loaded-file)
              (inferior-idris-send-command proc ":r")
            (setq inferior-idris-loaded-file relfile)
            (inferior-idris-send-command proc (concat ":l " relfile))))
      (error "Cannot find file for current buffer"))))

(defface idris-warning-face
  `((((class color) (background light))
     (:underline "orange"))
    (((class color) (background dark))
     (:underline "coral"))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'idris-faces)

(defvar idris-warnings '() "All warnings in the current buffer")

(defun get-region (warning)
  (goto-char (point-min))
  (let ((lno (string-to-number (nth 1 warning))))
    (values
     (line-beginning-position lno)
     (1- (line-beginning-position (1+ lno))))))

(defun get-message (warning)
  (nth 2 warning))

(defun idris-warning-overlay-p (overlay)
  (overlay-get overlay 'idris-warning))

(defun idris-warning-overlay-at-point ()
  "Return the overlay for a note starting at point, otherwise nil."
  (find (point) (remove-if-not 'idris-warning-overlay-p (overlays-at (point)))
        :key 'overlay-start))

(defun idris-warning-overlay (warning)
  "Add a compiler warning to the buffer as an overlay.
May merge overlays, if there's already one in the same location"
  (multiple-value-bind (start end) (get-region warning)
    (when start
      (goto-char start)
      (let ((overlay (idris-warning-overlay-at-point))
            (message (get-message warning)))
        (if overlay
            (idris-warning-merge-overlays overlay message)
          (idris-warning-create-overlay start end message))))))

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

(defun idris-higlight-warnings (warnings)
  "Highlight compiler warnings"
  (switch-to-buffer-other-window inferior-idris-loaded-buffer)
  (mapc #'delete-overlay idris-warnings)
  (setq idris-wanings '())
  (setq inferior-idris-look-for-warnings nil)
  (mapc #'idris-warning-overlay warnings))


(provide 'inferior-idris-mode)
;;; inferior-idris-mode.el ends here

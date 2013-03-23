;;; inferior-idris-mode.el --- Run an Idris interpreter inside of comint

;; Copyright (C) 2013  David Raymond Christiansen and Hannes Mehnert

;; Author: David Raymond Christiansen <drc@itu.dk>, Hannes Mehnert <hame@itu.dk>
;; Keywords: languages



;;; Commentary:

;; 

;;; Code:
(require 'comint)
(require 'idris-syntax)


(defconst idris-prompt-regexp "[^>]+> *")

(defcustom idris-interpreter-path "idris"
  "The path to the Idris interpreter"
  :type 'path) ; TODO customization group

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
        (make-comint "idris" idris-interpreter-path))
  (with-current-buffer inferior-idris-buffer (inferior-idris-mode)))

(defun inferior-idris-process ()
  "Return the Idris process or start it"
  (or (if (buffer-live-p inferior-idris-buffer)
          (get-buffer-process inferior-idris-buffer))
      (progn
        (call-interactively 'inferior-idris-start-process)
        (inferior-idris-process))))

(defun inferior-idris-send-command (proc str)
  (message (concat "sending str " str " to proc"))
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

(defun inferior-idris-spot-prompt (string)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (save-excursion
        (goto-char (process-mark proc))
        (if (re-search-backward (concat "^" idris-prompt-regexp) (line-beginning-position) t)
            (setq inferior-idris-seen-prompt t))))))

(defun inferior-idris-wait-for-prompt (proc &optional timeout)
  "Wait until PROC sends us a prompt.
The process PROC should be associated to a comint buffer."
  (with-current-buffer (process-buffer proc)
    (while (progn
             (goto-char comint-last-input-end)
             (not (or inferior-idris-seen-prompt
                      (setq inferior-idris-seen-prompt
                            (re-search-forward (concat "^" idris-prompt-regexp) nil t))
                      (not (accept-process-output proc timeout))))))
    (unless inferior-idris-seen-prompt
      (error "Can't find the prompt"))))

(defvar inferior-idris-working-directory nil
  "The current working directory of Idris")

(defun inferior-idris-load-file ()
  "Pass the current buffer's file to the inferior Idris process."
  (interactive)
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
          (inferior-idris-send-command proc (concat ":l " relfile)))
      (error "Cannot find file for current buffer"))))



(provide 'inferior-idris-mode)
;;; inferior-idris-mode.el ends here

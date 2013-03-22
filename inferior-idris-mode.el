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

(defconst inferior-idris-buffer-name "*inferior-idris*")

(define-derived-mode
    inferior-idris-mode comint-mode "Idris>"
    "Run an Idris interpreter."
    :syntax-table idris-syntax-table
    (setq comint-use-prompt-regexp t)
    (setq comint-prompt-regexp (concat "^" idris-prompt-regexp))
    (set (make-local-variable 'font-lock-defaults)
         idris-font-lock-defaults))


(defun inferior-idris ()
  "Run an inferior Idris process, or switch to the already running process"
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create inferior-idris-buffer-name))
  (inferior-idris-mode)
  (unless (comint-check-proc (get-buffer inferior-idris-buffer-name))
    (let ((idris-process
           (start-process "idris" (current-buffer) idris-interpreter-path)))
      (set-process-query-on-exit-flag idris-process t)
      (set-marker (process-mark idris-process) (point) (get-buffer inferior-idris-buffer-name)))))

(provide 'inferior-idris-mode)
;;; inferior-idris-mode.el ends here

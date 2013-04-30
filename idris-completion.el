;;; idris-completion.el --- Completion popup for Idris

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

(defvar idris-completions-buffer-name "*Completions*")

(defvar-local idris-completions-window nil
  "The window displaying *Completions* after saving window configuration.")

(defvar-local idris-complete-saved-window-configuration nil
  "Window configuration before we show the *Completions* buffer.
This is buffer local in the buffer where the completion is
performed.")

(defun idris-complete-maybe-save-window-configuration ()
  "Maybe save the current window configuration.
Return true if the configuration was saved."
  (unless (or idris-complete-saved-window-configuration
              (get-buffer-window idris-completions-buffer-name))
    (setq idris-complete-saved-window-configuration
          (current-window-configuration))
    t))

(defun idris-complete-delay-restoration ()
  (add-hook 'pre-command-hook
            'idris-complete-maybe-restore-window-configuration
            nil
            t))

(defun idris-complete-forget-window-configuration ()
  (setq idris-complete-saved-window-configuration nil)
  (setq idris-completions-window nil))

(defun idris-complete-restore-window-configuration ()
  "Restore the window config if available."
  (remove-hook 'pre-command-hook
               'idris-complete-maybe-restore-window-configuration)
  (when (and idris-complete-saved-window-configuration
             (idris-completion-window-active-p))
    (save-excursion
      (set-window-configuration
       idris-complete-saved-window-configuration))
    (setq idris-complete-saved-window-configuration nil)
    (when (buffer-live-p idris-completions-buffer-name)
      (kill-buffer idris-completions-buffer-name))))

(defun idris-complete-maybe-restore-window-configuration ()
  "Restore the window configuration, if the following command
terminates a current completion."
  (remove-hook 'pre-command-hook
               'idris-complete-maybe-restore-window-configuration)
  (condition-case err
      (cond ;((find last-command-char "()\"'`,# \r\n:")
            ; (idris-complete-restore-window-configuration))
            ((not (idris-completion-window-active-p))
             (idris-complete-forget-window-configuration))
            (t
             (idris-complete-delay-restoration)))
    (error
     ;; Because this is called on the pre-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in idris-complete-restore-window-configuration: %S" err))))

(defun idris-completion-window-active-p ()
  "Is the completion window currently active?"
  (and (window-live-p idris-completions-window)
       (equal (buffer-name (window-buffer idris-completions-window))
              idris-completions-buffer-name)))

(defun idris-display-completion-list (completions prefix partial)
  (let ((savedp (idris-complete-maybe-save-window-configuration))
        (sorted-completions (sort completions 'string-lessp)))
    (with-output-to-temp-buffer idris-completions-buffer-name
      (display-completion-list sorted-completions prefix)
      (let ((offset (- (point) 1 (length partial))))
        (with-current-buffer standard-output
          (setq completion-base-position offset))))
    (when savedp
      (setq idris-completions-window
            (get-buffer-window idris-completions-buffer-name)))))

(defun idris-display-or-scroll-completions (completions prefix partial)
  (if (and (eq last-command this-command)
           (idris-completion-window-active-p))
      (idris-scroll-completions)
    (idris-display-completion-list completions prefix partial))
  (idris-complete-delay-restoration))

(defun idris-scroll-completions ()
  (let ((window idris-completions-window))
    (with-current-buffer (window-buffer window)
       (if (pos-visible-in-window-p (point-max) window)
          (set-window-start window (point-min))
        (save-selected-window
          (select-window window)
          (scroll-up))))))

(provide 'idris-completion)

;;; idris-info.el --- Facilities for showing Idris help information -*- lexical-binding: t -*-

;; Copyright (C) 2014  David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module contains facilities for showing information provided by the
;; Idris compiler in a separate buffer, as well as keeping the irritation of
;; that buffer to a minimum.

;;; Code:
(require 'prop-menu)
(require 'idris-core)
(require 'idris-common-utils)
(require 'help-mode)

(defvar idris-info-buffer-name (idris-buffer-name :info)
  "The name of the buffer containing Idris help information")

(defvar idris-info-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map) ; remove the self-inserting char commands
    ;;; Allow buttons to be clicked with the left mouse button in info buffers
    (define-key map [follow-link] 'mouse-face)
    (cl-loop for keyer
             in '(idris-define-docs-keys
                  idris-define-general-keys
                  idris-define-active-term-keys)
             do (funcall keyer map))
    map))

(easy-menu-define idris-info-mode-menu idris-info-mode-map
  "Menu for the Idris info buffer."
  `("Idris Info"
    ["Show term interaction widgets" idris-add-term-widgets t]
    ["Close Idris info buffer" idris-info-quit t]))

(define-derived-mode idris-info-mode help-mode "Idris Info"
  "Major mode used for transient Idris information.
\\{idris-info-mode-map}
Invokes `idris-info-mode-hook'."
  (setq-local prop-menu-item-functions '(idris-context-menu-items))
  (set (make-local-variable 'prop-menu-item-functions) '(idris-context-menu-items)))

(defun idris-info-buffer ()
  "Return Idris info buffer."
  (let ((buffer (get-buffer-create idris-info-buffer-name)))
    (with-current-buffer buffer
      (when (not (eq major-mode 'idris-info-mode))
        (idris-info-mode)))
    buffer))

(defalias 'idris-info-quit #'quit-window)

(defmacro with-idris-info-buffer (&rest cmds)
  "Execute `CMDS' in a fresh Idris info buffer, then display it to the user."
  (declare (indent defun))
  `(idris-show-info (with-temp-buffer ,@cmds (buffer-string))))

(defun idris-show-info (info-string &optional spans)
  "Show INFO-STRING with SPANS in the Idris info buffer."
  (with-current-buffer (idris-info-buffer)
    ;; (help-xref-following t) ensure that current buffer -> idris-info-buffer
    ;; is recognised by `help-setup-xref' and `with-help-window'
    ;; as `help-buffer'
    (let ((help-xref-following t))
      (help-setup-xref (list #'idris-show-info info-string spans)
                       (called-interactively-p 'interactive))
      (with-help-window (current-buffer)
        (idris-propertize-spans (idris-repl-semantic-text-props spans)
          (insert info-string)))
      ;; reset major-mode for idris-info-buffer
      ;; back from help-mode to idris-info-mode
      (idris-info-buffer))))

(provide 'idris-info)
;;; idris-info.el ends here

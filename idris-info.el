;;; -*- lexical-binding: t -*-

;;; idris-info.el --- Facilities for showing Idris help information

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
(require 'idris-core)

(defvar idris-info-buffer-name (idris-buffer-name :info)
  "The name of the buffer containing Idris help information")

(defvar idris-info-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map) ; remove the self-inserting char commands
    (define-key map (kbd "q") 'idris-info-quit)
    map))

(easy-menu-define idris-info-mode-menu idris-info-mode-map
  "Menu for the Idris info buffer"
  `("Idris Info"
    ["Close Idris info buffer" idris-info-quit t]))

(define-derived-mode idris-info-mode fundamental-mode "Idris Info"
  "Major mode used for transient Idris information buffers
    \\{idris-info-mode-map}
Invokes `idris-info-mode-hook'."
  (view-mode 1))

(defun idris-info-buffer ()
  "Return the Idris info buffer, creating one if there is not one"
  (or (get-buffer idris-info-buffer-name)
      (let ((buffer (get-buffer-create idris-info-buffer-name)))
        (with-current-buffer buffer
          (idris-info-mode))
        buffer)))

(defun idris-info-quit ()
  (interactive)
  (when (get-buffer idris-info-buffer-name)
    (kill-buffer idris-info-buffer-name)))

(defun idris-info-buffer-visible-p ()
  (if (get-buffer-window idris-info-buffer-name 'visible) t nil))

(defun idris-show-info (info-string)
  "Show INFO-STRING in the Idris info buffer, obliterating its previous contents."
  (with-current-buffer (idris-info-buffer)
    (let ((inhibit-read-only t)) ; special variable - allows inserting into read-only buffer
      (erase-buffer)
      (insert (concat info-string "\n\n"))))
  (unless (idris-info-buffer-visible-p)
    (pop-to-buffer (idris-info-buffer))
    (message "Press q to close the Idris info buffer."))
  info-string)

(provide 'idris-info)
;;; idris-info.el ends here

;;; idris-highlight-input.el --- Compiler-driven highlighting of user input  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Keywords: languages

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

;; This file contains routines for highlighting user input with
;; information generated by the Idris elaborator.

;;; Code:

(require 'idris-common-utils)
(require 'idris-settings)

(defun idris-highlight-remove-overlays (&optional buffer)
  "Remove all Idris highlighting overlays from BUFFER.
Use the current buffer if BUFFER is nil."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (overlay-get overlay 'idris-source-highlight)
          (delete-overlay overlay))))))

(defun idris-highlight-column (idris-col)
  "Compute the Emacs position offset of the Idris column IDRIS-COL.

In particular, this takes bird tracks into account in literate Idris."
  (+ idris-col (if (idris-lidr-p) 1 -1)))

(defun idris-highlight--overlay-modification-hook (&rest args)
  "Delete semantic overlays if they are changed.

See Info node `(elisp)Overlay Properties' to understand how ARGS are used."
  ;; There are 5 args when it's called post-modification
  (when (= (length args) 5)
    (let ((overlay (car args)))
      (delete-overlay overlay))))

(defun idris-highlight-input-region (buffer start-line start-col end-line end-col highlight)
  "Highlight in BUFFER using an overlay from START-LINE and START-COL to
 END-LINE and END-COL and the semantic properties specified in HIGHLIGHT."
  (when idris-semantic-source-highlighting
    (save-restriction
      (widen)
      (if (or (> end-line start-line)
              (and (= end-line start-line)
                   (> end-col start-col)))
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-min))
              (let* ((start-pos (+ (line-beginning-position start-line)
                                   (idris-highlight-column start-col)))
                     (end-pos (+ (line-beginning-position end-line)
                                 (idris-highlight-column end-col)))
                     (highlight-overlay (make-overlay start-pos end-pos
                                                      (get-buffer buffer))))
                (overlay-put highlight-overlay 'idris-source-highlight t)
                (idris-add-overlay-properties highlight-overlay
                                              (idris-semantic-properties highlight))
                (overlay-put highlight-overlay
                             'modification-hooks
                             '(idris-highlight--overlay-modification-hook)))))
        (when (eq idris-semantic-source-highlighting 'debug)
          (message "Not highlighting absurd span %s:%s-%s:%s with %s"
                   start-line start-col
                   end-line end-col
                   highlight ))))))


(provide 'idris-highlight-input)
;;; idris-highlight-input.el ends here

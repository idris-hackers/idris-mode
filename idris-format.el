;;; idris-format.el --- Commands to format/prettify Idris code -*- lexical-binding: t -*-

;; Copyright (C) 2023 Marek L.

;; Author: Marek L. <nospam.keram@gmail.com>

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
;; Commands to prettify Idris code

;;; Code:

(require 'rx)
(require 'subr-x)
(require 'cl-seq)

(defun idris-split-line-to-lhs-rhs (line)
  "Split LINE string to two parts starting at first occurence of equal sign (=).
The equal sign is part of the right hand side. If no equal sign found
the `car' of result (LHS) is empty string and `cdr' of result (RHS) the LINE."
  (let* ((lhs-length (or (string-match "=" line) 0))
         (lhs (string-trim-right (string-limit line lhs-length)))
         (rhs (substring line lhs-length)))
    (cons lhs rhs)))

(defun idris-mapcar-with-index (fn seq)
  "Apply function FN to each element of SEQ, and make a list of the results.
First argument to FN is element of SEQ and second its position in the SEQ."
  (let ((i -1))
    (mapcar (lambda (val)
              (funcall fn val (cl-incf i)))
            seq)))

(defun idris-transpose-matrix (matrix)
  "Convert MATRIX fro MxN to NxM elements."
  (cl-labels ((trans-acc (m rows)
                         (if (null (remove nil m))
                             (reverse rows)
                           (trans-acc (mapcar 'cdr m)
                                      (cons (mapcar 'car m) rows)))))
    (trans-acc matrix '())))

(defun idris-parens-balanced-p (str)
  "Return t if all parens in STR are balanced."
  (eq (length (replace-regexp-in-string (rx (any "(" "[" "{")) "" str))
      (length (replace-regexp-in-string (rx (any ")" "]" "}")) "" str))))

(defun idris-max-words-lengths (split-lines)
  "Return list of max lengths for each column in SPLIT-LINES."
  (mapcar (lambda (words)
            (apply #'max (mapcar #'length words)))
          (idris-transpose-matrix split-lines)))

(defun idris-split-words (str)
  "Split string STR to list of words.
As word is considered a sequence of non blank characters
or expression inside parenthesis (including parenthesis)

Example: (idris-split-words \"foo (S k) k\")
 => '(\"foo\" \"(S k)\" \"k\")"
  (let ((acc '())
        (open '()))
    (mapc
     (lambda (word)
       (if (null open)
           (if (string-match "^[([{]" word)
               (push word open)
             (push word acc))
         (push word open)
         (and (idris-parens-balanced-p (string-join open))
              (push (string-join (reverse open) " ") acc)
              (setq open '())
              (push word open))))
     (split-string str))
    (reverse acc)))

(defun idris-pad-lines (lines)
  "Return LINES with equal length of left hand side."
  (let* ((prev-lhs-length)
         (lhs-rhss
          (mapcar (lambda (line)
                    (if (and prev-lhs-length
                             (< prev-lhs-length
                                (- (length line) (length (string-trim-left line)))))
                        (cons nil line)
                      (let ((lhs-rhs (idris-split-line-to-lhs-rhs line)))
                        (setq prev-lhs-length (length (car lhs-rhs)))
                        lhs-rhs)))
                  lines))
         (lhss-words (mapcar #'idris-split-words (delq nil (mapcar #'car lhs-rhss))))
         (mvl (idris-max-words-lengths lhss-words))
         (prev-lhs-delta))
    (mapcar
     (lambda (lhs-rhs)
       (if (and prev-lhs-delta (null (car lhs-rhs)))
           (concat (make-string prev-lhs-delta ? ) (cdr lhs-rhs))
         (let ((new-lhs (string-join (idris-mapcar-with-index (lambda (word j)
                                                                (string-pad word (nth j mvl)))
                                                              (car lhss-words))
                                     " ")))
           (setq prev-lhs-delta (- (length new-lhs) (length (car lhs-rhs))))
           (setq lhss-words (cdr lhss-words))
           (concat new-lhs " " (cdr lhs-rhs)))))
     lhs-rhss)))

(defun idris-end-of-clause ()
  "Return position at the end of clause.

As end of clause is considered end of non empty line before empty line
or end of buffer."
  (save-excursion
    (condition-case _err
        (progn
          (re-search-forward (rx (or (and "\n" (zero-or-more blank) "\n")
                                     string-end)))
          (re-search-backward (rx (one-or-more not-newline)))
          (line-end-position))
      (error (user-error "End of clause not found")))))

(defun idris-beginning-of-clause ()
  "Return position at the end of clause.

As beginning of clause is considered beginning of top most line
before line containing colon symbol (:) that contains equal sign (=)."
  (save-excursion
    (condition-case _err
        (progn
          (re-search-backward ":")
          (re-search-forward "=")
          (beginning-of-line)
          (skip-chars-forward " \t")
          (point))
      (error (user-error "Beginning of clause not found")))))

(defun idris-clause-point-column (point)
  "Return column of the POINT."
  (save-excursion
    (goto-char point)
    (current-column)))

;; Maybe we could leverage existing `align' library?
(defun idris-format-align-lhs ()
  "Align LHS of function definition."
  (interactive)
  (let ((init-position (point)))
    (beginning-of-line)
    (let* ((start (idris-beginning-of-clause))
           (end (idris-end-of-clause))
           (indent (make-string (idris-clause-point-column start) ? ))
           (lines (string-lines (buffer-substring-no-properties start end)))
           (padded-lines (mapcar (lambda (line) (concat indent line))
                                 (idris-pad-lines lines)))
           (new-region (string-join padded-lines "\n")))
      (goto-char start)
      (beginning-of-line)
      (delete-region (point) end)
      (insert new-region)
      (goto-char init-position))))

(provide 'idris-format)

;;; idris-format.el ends here

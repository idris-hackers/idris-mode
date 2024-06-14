;;; idris-format-test.el --- Tests for prettify-ing commands of Idris code -*- lexical-binding: t -*-

;; Copyright (C) 2023 Marek L.

;; Author: Marek L. <nospam.keram@gmail.com>

;; License:

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

;; Collection of tests for interactive commands to prettify Idris source code

;;; Code:

(let ((test-dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path test-dir)
  ;; In batch mode default dir points to ../ and causing issues with saving
  ;; Idris fixtures so we set it to the test-dir to avoid the issues.
  (setq default-directory test-dir))

(require 'idris-format)
(require 'ert)
(require 'cl-lib)

(defun idris-buffer-content ()
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest idris-format-align-lhs ()
  (with-temp-buffer
    (let ((payload (string-join '("minus : Nat -> Nat -> Nat"
                                  "minus 0 j = ?minus_rhs_0"
                                  "minus (S k) j = ?minus_rhs_1")
                                "\n"))
          (expected (string-join '("minus : Nat -> Nat -> Nat"
                                   "minus 0     j = ?minus_rhs_0"
                                   "minus (S k) j = ?minus_rhs_1")
                                 "\n"))
          (position-re "0"))
      (erase-buffer)
      (insert payload)
      (goto-char (point-min))
      (re-search-forward position-re)
      (idris-format-align-lhs)
      (should (string-equal expected (idris-buffer-content))))))

(ert-deftest idris-format-align-lhs-with-point-at-end-of-the-clause ()
  (with-temp-buffer
    (let ((payload (string-join '("minus : Nat -> Nat -> Nat"
                                  "minus 0 j = ?minus_rhs_0"
                                  "minus (S k) j = ?minus_rhs_1")
                                "\n"))
          (expected (string-join '("minus : Nat -> Nat -> Nat"
                                   "minus 0     j = ?minus_rhs_0"
                                   "minus (S k) j = ?minus_rhs_1")
                                 "\n"))
          (position-re "rhs_1"))
      (erase-buffer)
      (insert payload)
      (goto-char (point-min))
      (re-search-forward position-re)
      (idris-format-align-lhs)
      (should (string-equal expected (idris-buffer-content))))))

(ert-deftest idris-format-align-lhs-with-point-at-beginning-of-clause ()
  (with-temp-buffer
    (let ((payload (string-join '("minus : Nat -> Nat -> Nat"
                                  "minus 0 j = ?minus_rhs_0"
                                  "minus (S k) j = ?minus_rhs_1")
                                "\n"))
          (expected (string-join '("minus : Nat -> Nat -> Nat"
                                   "minus 0     j = ?minus_rhs_0"
                                   "minus (S k) j = ?minus_rhs_1")
                                 "\n"))
          (position-re "0"))
      (erase-buffer)
      (insert payload)
      (goto-char (point-min))
      (re-search-forward position-re)
      (beginning-of-line)
      (idris-format-align-lhs)
      (should (string-equal expected (idris-buffer-content))))))

(ert-deftest idris-format-align-lhs-with-point-at-second-line-with-multiline-type-definition ()
  (with-temp-buffer
    (let ((payload (string-join '("minus : Nat"
                                  "  -> Nat"
                                  "  -> Nat"
                                  "minus 0 j = ?minus_rhs_0"
                                  "minus (S k) j = ?minus_rhs_1")
                                "\n"))
          (expected (string-join '("minus : Nat"
                                   "  -> Nat"
                                   "  -> Nat"
                                   "minus 0     j = ?minus_rhs_0"
                                   "minus (S k) j = ?minus_rhs_1")
                                 "\n"))
          (position-re "0"))
      (erase-buffer)
      (insert payload)
      (goto-char (point-min))
      (re-search-forward position-re)
      (beginning-of-line)
      (idris-format-align-lhs)
      (should (string-equal expected (idris-buffer-content))))))

(ert-deftest idris-format-align-lhs-indented ()
  (with-temp-buffer
    (let ((payload (string-join '("minus : Nat -> Nat -> Nat"
                                  "  minus 0 j = ?minus_rhs_0"
                                  "  minus (S k) j = ?minus_rhs_1")
                                "\n"))
          (expected (string-join '("minus : Nat -> Nat -> Nat"
                                   "  minus 0     j = ?minus_rhs_0"
                                   "  minus (S k) j = ?minus_rhs_1")
                                 "\n"))
          (position-re "0"))
      (erase-buffer)
      (insert payload)
      (goto-char (point-min))
      (re-search-forward position-re)
      (beginning-of-line)
      (idris-format-align-lhs)
      (should (string-equal expected (idris-buffer-content))))))

(ert-deftest idris-format-align-lhs-with-multiline-rhs ()
  (with-temp-buffer
    (let ((payload (string-join '("minus : Nat -> Nat -> Nat"
                                  "minus 0 j = case j < 3 of"
                                  "                 False => ?minus_rhs_2"
                                  "                 True => ?minus_rhs_3"
                                  "minus (S k) j = ?minus_rhs_1")
                                "\n"))
          (expected (string-join '("minus : Nat -> Nat -> Nat"
                                   "minus 0     j = case j < 3 of"
                                   "                     False => ?minus_rhs_2"
                                   "                     True => ?minus_rhs_3"
                                   "minus (S k) j = ?minus_rhs_1")
                                 "\n"))
          (position-re "case"))
      (erase-buffer)
      (insert payload)
      (goto-char (point-min))
      (re-search-forward position-re)
      (idris-format-align-lhs)
      (should (string-equal expected (idris-buffer-content))))))

(ert-deftest idris-format-align-lhs-with-nested-parenthesis ()
  (with-temp-buffer
    (let ((payload (string-join '("minus : Nat -> Nat -> Nat"
                                  "minus 0 j = ?minus_rhs_0"
                                  "minus (T (S k) (S l)) j = ?minus_rhs_1")
                                "\n"))
          (expected (string-join '("minus : Nat -> Nat -> Nat"
                                   "minus 0               j = ?minus_rhs_0"
                                   "minus (T (S k) (S l)) j = ?minus_rhs_1")
                                 "\n"))
          (position-re "0"))
      (erase-buffer)
      (insert payload)
      (goto-char (point-min))
      (re-search-forward position-re)
      (idris-format-align-lhs)
      (should (string-equal expected (idris-buffer-content))))))

(ert-deftest idris-format-align-lhs-with-square-brackets ()
  (with-temp-buffer
    (let ((payload (string-join '("minus : Nat -> Nat -> Nat"
                                  "minus 0 j = ?minus_rhs_0"
                                  "minus [T (S k), (S l)] j = ?minus_rhs_1")
                                "\n"))
          (expected (string-join '("minus : Nat -> Nat -> Nat"
                                   "minus 0                j = ?minus_rhs_0"
                                   "minus [T (S k), (S l)] j = ?minus_rhs_1")
                                 "\n"))
          (position-re "0"))
      (erase-buffer)
      (insert payload)
      (goto-char (point-min))
      (re-search-forward position-re)
      (idris-format-align-lhs)
      (should (string-equal expected (idris-buffer-content))))))

(provide 'idris-format-test)

;;; idris-format-test.el ends here

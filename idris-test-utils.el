;;; idris-tests.el --- Tests utilty for idris-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Yasuhiko Watanabe

;; Author: Yasuhiko Watanabe
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

;; This is a collection of simple tests for idris-mode.

;;; Code:
(require 'cl-lib)
(require 'idris-mode)

(defun idris-test-run-goto-char (test-fun &rest args)
  "To run commands like idris-case-split, we have to move the cursor to an appropriate location.
This funtion moves the cursor to the next line of first + inside comment starting on the first column.

module Test
data Cases = A | B
test : Cases -> Cases
--    +++
test  var = ?cases_rhs
      ^
      |
      +--------- cursor moves here and apply test-fun to args
"
  (progn
    (goto-char (point-min))
    (search-forward-regexp "^--.*\\+")
    (goto-char (- (point) 1))
    (next-line)
    (apply test-fun args)
    ))


;; Under consideration.
(defmacro idris-ert-command-action (test-case test-fun buffer-p)
  "Common test code to run idris-mode command which modifies the buffer.
It is used for the command
- if command succeeds, it modifies buffer and stays in original buffer.
- otherwise test failure
As this is not for a test of Idris itself, we do not care the results."
  `(ert-deftest ,(intern (concat (symbol-name test-fun) "/" (string-remove-suffix ".idr" test-case))) ()
			  
     (let ((buffer (find-file ,test-case)))
       (with-current-buffer buffer
	 (idris-load-file)
	 (dotimes (_ 5) (accept-process-output nil 0.1)) ;;
	 (idris-test-run-goto-char (function ,test-fun))
	 (let ((this-buffer (current-buffer)))
	   (should (,buffer-p buffer this-buffer))))
	 (kill-buffer))
     (idris-quit)))

(defmacro idris-ert-command-action2 (test-case test-fun buffer-p)
  "Common test code to run idris-mode command which modifies the buffer.
It is used for the command
- if command succeeds, it modifies buffer and stays in original buffer.
- otherwise test failure
As this is not for a test of Idris itself, we do not care the results."
  `(ert-deftest ,(intern (concat (symbol-name test-fun) "/" (string-remove-suffix ".idr" test-case))) ()
			  
     (let ((buffer (find-file ,test-case)))
       (with-current-buffer buffer
	 (idris-load-file)
	 (dotimes (_ 5) (accept-process-output nil 0.1)) ;;
	 (idris-test-run-goto-char (function ,test-fun) nil)
	 (let ((this-buffer (current-buffer)))
	   (should (,buffer-p buffer this-buffer))))
	 (kill-buffer))
     (idris-quit)))


(defun idris-test-eq-buffer (orig modified)
  (and (buffer-modified-p orig) (eq orig modified)))

(defmacro check-rest (&rest args)
  `(listp (quote ,args)))

(provide 'idris-test-utils)
;;; idris-test-utils.el ends here

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



(provide 'idris-test-utils)
;;; idris-test-utils.el ends here

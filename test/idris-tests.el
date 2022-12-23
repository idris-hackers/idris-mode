;;; idris-tests.el --- Tests for idris-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2014  David Raymond Christiansen

;; Author: David Raymond Christiansen <drc@itu.dk>
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
;; Structuring:
;; Packages modules (files) have their own corresponding test files.
;; idris-commands.el -> idris-commands-test.el
;; idris-navigate.el -> idris-navigate-test.el
;; ..
;; Tests for modules that are too small or not yet covered enough by tests
;; can be left as part of the idris-tests.el
;;
;; Naming tests:
;; `ert-deftest idris-X-Y`
;; where X stands for function or state to be tested
;; and Y for additional context or behaviour.
;; Example:
;; `ert-deftest idris-quit`
;; `ert-deftest idris-quit-logging-enabled`

;;; Code:

;; Implementations
(require 'idris-mode)

;; Testing
;; load-file-name is present in batch mode and buffer-file-name in interactive
(let ((test-dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path test-dir)
  ;; In batch mode default dir points to ../ and causing issues with saving
  ;; Idris fixtures so we set it to the test-dir to avoid the issues.
  (setq default-directory test-dir))

(require 'ert)

(ert-deftest trivial-test ()
  (should t))

(ert-deftest idris-editor-port ()
  (let ((output "Can't find import Prelude\n37072\n"))
    (should (string-match idris-process-port-output-regexp output))
    (should (string= "Can't find import Prelude\n" (match-string 1 output)))
    (should (string= "37072" (match-string 2 output))))
  (let ((output "37072\n"))
    (should (string-match idris-process-port-output-regexp output))
    (should (null (match-string 1 output)))
    (should (string= "37072" (match-string 2 output)))))

(ert-deftest idris-test-find-cmdline-args ()
  "Test that idris-mode calculates command line arguments from .ipkg files."
  ;; Outside of a project, none are found
  (let ((buffer (find-file "test-data/ProofSearch.idr")))
    (with-current-buffer buffer
      (should (null (idris-ipkg-flags-for-current-buffer)))
      (kill-buffer)))
  ;; Inside of a project, the correct ones are found
  (let ((buffer (find-file "test-data/cmdline/src/Command/Line/Test.idr")))
    (with-current-buffer buffer
      (should (equal (idris-ipkg-flags-for-current-buffer)
                     (list "-p" "effects")))
      (kill-buffer))))

(ert-deftest idris-test-error-buffer ()
  "Test that loading a type-incorrect Idris buffer results in an error message buffer."
  (let ((buffer (find-file "test-data/TypeError.idr")))
    (with-current-buffer buffer
      (idris-load-file)
      (dotimes (_ 5) (accept-process-output nil 0.1))
      (should (get-buffer idris-notes-buffer-name)))
    (with-current-buffer (get-buffer idris-notes-buffer-name)
      (goto-char (point-min))
      (should (re-search-forward "Nat" nil t))) ;; check that the buffer has something error-like
    (with-current-buffer buffer
      (kill-buffer))
    (idris-quit)))

(ert-deftest idris-test-ipkg-packages-with-underscores-and-dashes ()
  "Test that loading an ipkg file can have dependencies on packages with _ or - in the name."
  (let ((buffer (find-file "test-data/package-test/Packaging.idr")))
    (with-current-buffer buffer
      (should (equal '("-p" "idris-free" "-p" "recursion_schemes")
                     (idris-ipkg-pkgs-flags-for-current-buffer)))
      (kill-buffer buffer))
    (idris-quit)))

(ert-deftest idris-test-warning-overlay ()
  "Test that `idris-warning-overaly-point' works as expected."
  (let* ((buffer (find-file-noselect "test-data/AddClause.idr"))
         (warning '("AddClause.idr" (5 7) (5 17) "Some warning message" ()))
         (idris-raw-warnings '())
         (idris-process-current-working-directory (file-name-directory (buffer-file-name buffer)))
         (expected-position)
         (expected-overlay))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "data Test")
      (setq expected-position (point))

      (idris-warning-overlay warning)

      ;; Assert that the point position does not change
      ;; https://github.com/idris-community/idris2-mode/issues/36
      (should (eq (point) expected-position))

      ;; Assert side effect
      (should (not (null idris-raw-warnings)))

      ;; Assert that overlay was added
      (setq expected-overlay (car (overlays-in (point-min) (point-max))))
      (should (not (null expected-overlay)))
      (should (string= (overlay-get expected-overlay 'help-echo)
                       "Some warning message"))
      ;; Cleanup
      (idris-delete-ibc t)
      (kill-buffer))))

(load "idris-commands-test")
(load "idris-navigate-test")
(load "idris-xref-test")

(provide 'idris-tests)
;;; idris-tests.el ends here

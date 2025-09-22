;;; idris-commands-test.el --- Tests for interactive commands  -*- lexical-binding: t -*-

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

;; This is a collection of tests for interactive commands in idris-mode.

;;; Code:

;; to load idris-test-utils
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-dir))

(require 'idris-commands)
(require 'idris-test-utils)

(require 'ert)
(require 'cl-lib)

(defun normalised-buffer-list ()
  "Buffer list without randomly appearing internal buffer(s)."
  (cl-delete-if (lambda (b) (string-match-p "code-conversion-work" (buffer-name b)))
                (buffer-list)))

(ert-deftest idris-test-idris-run ()
  (let ((buffer (find-file "test-data/Empty.idr")))
    (should buffer)
    (with-current-buffer buffer
      (idris-run)
      (dotimes (_ 10) (accept-process-output nil 0.1))
      (should idris-process)
      (should idris-connection))
    (idris-delete-ibc t)
    (kill-buffer))
  (idris-quit))

(ert-deftest idris-test-idris-quit ()
  "Ensure that running Idris and quitting doesn't leave behind unwanted buffers."
  (let ((before (normalised-buffer-list))
        (idris-log-events nil))
    (idris-repl)
    (dotimes (_ 10) (accept-process-output nil 0.1))
    (idris-quit)
    (let* ((after (normalised-buffer-list))
           (extra (cl-set-difference after before)))
      (should (= (length extra) 0)))))

(ert-deftest idris-test-idris-quit-logging-enabled ()
  "Ensure that running Idris and quitting doesn't leave behind unwanted buffers.
In particular, only *idris-events* should remain."
  (let ((before (normalised-buffer-list))
        (idris-event-buffer-name "*idris-test-idris-events*")
        (idris-log-events t)
        (expected-difference ))
    (idris-repl)
    (dotimes (_ 10) (accept-process-output nil 0.1))
    (idris-quit)
    (let* ((after (normalised-buffer-list))
           (extra (cl-set-difference after before)))
      (should (= (length extra) 1))
      (should (string= (buffer-name (car extra)) idris-event-buffer-name)))

    ;; Cleanup
    (kill-buffer idris-event-buffer-name)))

(ert-deftest idris-load-file-idris-hole-show-on-load-enabled ()
  "Test that the holes buffer is created."
  (let ((buffer (find-file-noselect "test-data/MetavarTest.idr"))
        (idris-hole-show-on-load t))
    (with-current-buffer buffer
      (idris-load-file)

      ;; Allow async stuff to happen
      (dotimes (_ 10) (accept-process-output nil 0.1))
      (let ((mv-buffer (get-buffer idris-hole-list-buffer-name)))
        ;; The buffer exists and contains characters
        (should (bufferp mv-buffer))
        (should (> (buffer-size mv-buffer) 10))
        (kill-buffer mv-buffer))

      ;; Clean up
      (with-current-buffer buffer
        (idris-delete-ibc t)
        (kill-buffer))))

  ;; More cleanup
  (idris-quit))

(ert-deftest idris-load-file-idris-hole-show-on-load-disabled ()
  "Test that holes buffer is not created."
  (let ((buffer (find-file-noselect "test-data/MetavarTest.idr"))
        (idris-hole-show-on-load nil))
    (with-current-buffer buffer
      (idris-load-file))
    (dotimes (_ 10) (accept-process-output nil 0.1))
    (let ((mv-buffer (get-buffer idris-hole-list-buffer-name)))
      (should-not (bufferp mv-buffer))
      (should (null mv-buffer)))

    ;; Clean up
    (with-current-buffer buffer
      (idris-delete-ibc t)
      (kill-buffer)))

  ;; More cleanup
  (idris-quit))

(ert-deftest idris-list-holes ()
  "Test `idris-list-holes' command."
  (let ((other-buffer (find-file-noselect "test-data/MakeWithBlock.idr"))
        (buffer (find-file-noselect "test-data/MetavarTest.idr")))

    ;; Test that hole info is present without need to load file manually
    (with-current-buffer buffer
      (idris-list-holes)
      (dotimes (_ 10) (accept-process-output nil 0.1))
      (let ((holes-buffer (get-buffer idris-hole-list-buffer-name)))
        (should (bufferp holes-buffer))
        (should (> (buffer-size holes-buffer) 10)))
      (idris-delete-ibc t))

    ;; Test that the hole info is updated for the other current buffer
    (with-current-buffer other-buffer
      (idris-list-holes)
      (dotimes (_ 10) (accept-process-output nil 0.1))
      (let ((holes-buffer (get-buffer idris-hole-list-buffer-name)))
        (should (not (bufferp holes-buffer))))
      (idris-delete-ibc t))

    (kill-buffer buffer)
    (kill-buffer other-buffer)
    (idris-quit)))

(when (string-match-p "idris$" idris-interpreter-path)
  (ert-deftest idris-test-proof-search ()
    "Test that proof search works."
    :expected-result (if (string-match-p "idris2$" idris-interpreter-path)
                         :failed
                       :passed)
    (skip-unless (string-match-p "idris$" idris-interpreter-path))

    (let ((buffer (find-file "test-data/ProofSearch.idr")))
      (with-current-buffer buffer
        (idris-load-file)
        (dotimes (_ 10) (accept-process-output nil 0.1))
        (goto-char (point-min))
        (re-search-forward "search_here")
        (goto-char (match-beginning 0))
        (idris-proof-search)
        (dotimes (_ 10) (accept-process-output nil 0.1))
        (should (looking-at-p "lteSucc (lteSucc (lteSucc (lteSucc (lteSucc lteZero))))"))
        (move-beginning-of-line nil)
        (delete-region (point) (line-end-position))
        (insert "prf = ?search_here")
        (save-buffer)
        (idris-delete-ibc t)
        (kill-buffer)))

    ;; More cleanup
    (idris-quit)))

(ert-deftest idris-test-idris-type-search ()
  "Test that `idris-type-search' produces output in Idris info buffer."
  (idris-run)
  (funcall-interactively 'idris-type-search "Nat")
  (with-current-buffer (get-buffer idris-info-buffer-name)
    (goto-char (point-min))
    (should (re-search-forward "Zero" nil t)))
  (idris-quit))

(ert-deftest idris-test-idris-add-clause ()
  "Test that `idris-add-clause' generates definition with hole."
  (let ((buffer (find-file-noselect "test-data/AddClause.idr"))
        (buffer-content (with-temp-buffer
                          (insert-file-contents "test-data/AddClause.idr")
                          (buffer-string))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "test :")
      (goto-char (match-beginning 0))
      (funcall-interactively 'idris-add-clause nil)
      (should (looking-at-p "test \\w+ = \\?test_rhs"))
      (idris-delete-ibc t)

      (re-search-forward "(-) :")
      (goto-char (1+ (match-beginning 0)))
      (funcall-interactively 'idris-add-clause nil)
      (should (looking-at-p "(-) = \\?\\w+_rhs"))
      (idris-delete-ibc t)

      ;; Test that response with indentation (Idris2) are aligned correctly
      ;; Idris1 response: "revAcc xs ys = ?revAcc_rhs"
      ;; Idris2 response: "  revAcc xs ys = ?revAcc_rhs"
      (goto-char (point-max))
      (insert "
myReverse : List a -> List a
myReverse xs = revAcc [] xs where
  revAcc : List a -> List a -> List a")
      (search-backward "evAcc")
      (funcall-interactively 'idris-add-clause nil)
      (beginning-of-line)
      (should (looking-at-p "^  revAcc xs ys = \\?revAcc_rhs"))

      ;; Cleanup
      (erase-buffer)
      (insert buffer-content)
      (save-buffer)
      (kill-buffer))
    (idris-quit)))

(ert-deftest idris-test-idris-refine ()
  "Test that `idris-refine' works as expected."
  (let* ((buffer (find-file "test-data/Refine.idr"))
         (buffer-content (buffer-substring-no-properties (point-min) (point-max))))
    (goto-char (point-min))
    (search-forward "test : T")
    (beginning-of-line)
    (funcall-interactively 'idris-add-clause nil)
    (should (looking-at-p "test \\w+ = \\?test_rhs"))
    (idris-delete-ibc t)
    (search-forward "?test")
    (funcall-interactively 'idris-refine "x")
    (should (looking-at-p
             (if (>=-protocol-version 2 1)
                 "x"
               "?test_rhs1")))

    ;; Cleanup
    (idris-delete-ibc t)
    (erase-buffer)
    (insert buffer-content)
    (save-buffer)
    (kill-buffer)
    (idris-quit)))

(ert-deftest idris-test-idris-type-at-point ()
  "Test that `idris-type-at-point' works."
  (let ((buffer (find-file-noselect "test-data/AddClause.idr"))
        file-loaded-p
        eval-args)
    (cl-flet ((idris-load-file-sync-stub () (setq file-loaded-p t) nil)
              (idris-eval-stub (&optional &rest args)
                               (setq eval-args args)
                               '("Test : Type"
                                (0 4 ((:name "AddClause.Test")
                                      (:implicit :False)
                                      (:key "AQAAAAAAAAAA")
                                      (:decor :type)
                                      (:doc-overview "")
                                      (:type "Type")
                                      (:namespace "AddClause")))
                                (7 4 ((:decor :type)
                                      (:type "Type")
                                      (:doc-overview "The type of types")
                                      (:name "Type")))
                                (7 4 ((:tt-term "AAAAAAAAAAAHAAAAAAA"))))))
      (advice-add 'idris-load-file-sync :override #'idris-load-file-sync-stub)
      (advice-add 'idris-eval :override #'idris-eval-stub)
      (unwind-protect
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (goto-char (point-min))
            (re-search-forward "data Test")
            (funcall-interactively 'idris-type-at-point nil)
            (should (eq file-loaded-p t))
            (should (equal eval-args '((:type-of "Test"))))
            (with-current-buffer idris-info-buffer-name
              (should (string-match-p "Test : Type" (buffer-string)))))

        (advice-remove 'idris-load-file-sync #'idris-load-file-sync-stub)
        (advice-remove 'idris-eval #'idris-eval-stub)
        (kill-buffer buffer)
        (idris-quit)))))

(ert-deftest idris-test-idris-start-project ()
  "Test generating valid .ipkg file."
  (let ((mock-project-name "TestProject")
        (mock-package-file-name "test-project.ipkg")
        (mock-source-directory "src")
        (mock-first-module "TestModule")
        (mock-directory-name "test-start-project"))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (prompt &rest _)
                     (cond ((string-prefix-p "Project name:" prompt)
                            mock-project-name)
                           ((string-prefix-p "Package file name" prompt)
                            mock-package-file-name)
                           ((string-prefix-p "Source directory" prompt)
                            mock-source-directory)
                           ((string-prefix-p "First module name" prompt)
                            mock-first-module))))
                  ((symbol-function 'read-directory-name)
                   (lambda (&rest _) mock-directory-name)))
          (idris-start-project)
          (with-current-buffer mock-package-file-name
            (goto-char (point-min))
            (should (search-forward "package test-project"))
            (should (search-forward "opts = \"\""))
            (should (search-forward "sourcedir = \"src\""))
            (should (search-forward "modules = TestModule"))
            (kill-buffer)))
      (if (get-buffer (concat mock-first-module ".idr"))
          (kill-buffer (concat mock-first-module ".idr")))
      (delete-directory mock-directory-name t)
      (idris-quit))))

(ert-deftest idris-test-idris-make-lemma ()
  "Test `idris-make-lemma' replacing a hole with a metavariable lemma."
  (cl-flet ((idris-load-file-sync-stub () nil)
            (idris-eval-stub (&optional &rest args)
              '((:metavariable-lemma
                 (:replace-metavariable "closeDistance_rhs s1 s2")
                 (:definition-type "closeDistance_rhs : String -> String -> IO Bool")))))
    (advice-add 'idris-load-file-sync :override #'idris-load-file-sync-stub)
    (advice-add 'idris-eval :override #'idris-eval-stub)
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = ?closeDistance_rhs")
            (goto-char (point-min))
            (re-search-forward "closeDistance_rh")
            (funcall-interactively 'idris-make-lemma)
            (should (string= "closeDistance_rhs : String -> String -> IO Bool

closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = closeDistance_rhs s1 s2"
                             (buffer-substring-no-properties (point-min) (point-max)))))

          (with-temp-buffer
            (insert "something_else

closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = ?closeDistance_rhs")
            (goto-char (point-min))
            (re-search-forward "closeDistance_rh")
            (funcall-interactively 'idris-make-lemma)
            (should (string= "something_else

closeDistance_rhs : String -> String -> IO Bool

closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = closeDistance_rhs s1 s2"
                             (buffer-substring-no-properties (point-min) (point-max)))))

          (with-temp-buffer
            (insert "something_else

closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = ?closeDistance_rhs")
            (goto-char (point-min))
            (re-search-forward "closeDistance_rh")
            (funcall-interactively 'idris-make-lemma)
            (should (string= "something_else

closeDistance_rhs : String -> String -> IO Bool

closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = closeDistance_rhs s1 s2"
                             (buffer-substring-no-properties (point-min) (point-max)))))

          (with-temp-buffer
            (insert "||| Check if two strings are close enough to be similar,
||| using the namespace distance criteria.
closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = ?closeDistance_rhs")
            (goto-char (point-min))
            (re-search-forward "closeDistance_rh")
            (funcall-interactively 'idris-make-lemma)
            (should (string= "closeDistance_rhs : String -> String -> IO Bool

||| Check if two strings are close enough to be similar,
||| using the namespace distance criteria.
closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = closeDistance_rhs s1 s2"
                             (buffer-substring-no-properties (point-min) (point-max)))))

          (with-temp-buffer
            (insert "something_else

||| Check if two strings are close enough to be similar,
||| using the namespace distance criteria.
closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = ?closeDistance_rhs")
            (goto-char (point-min))
            (re-search-forward "closeDistance_rh")
            (funcall-interactively 'idris-make-lemma)
            ;; (message "%s" (buffer-substring-no-properties (point-min) (point-max)))
            (should (string= "something_else

closeDistance_rhs : String -> String -> IO Bool

||| Check if two strings are close enough to be similar,
||| using the namespace distance criteria.
closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = closeDistance_rhs s1 s2"
                             (buffer-substring-no-properties (point-min) (point-max)))))

          (with-temp-buffer
            (insert "something else

-- some inline comment
closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = ?closeDistance_rhs")
            (goto-char (point-min))
            (re-search-forward "closeDistance_rh")
            (funcall-interactively 'idris-make-lemma)
            (should (string= "something else

closeDistance_rhs : String -> String -> IO Bool

-- some inline comment
closeDistance : String -> String -> IO Bool
closeDistance s1 s2 = closeDistance_rhs s1 s2"
                             (buffer-substring-no-properties (point-min) (point-max))))))

      (advice-remove 'idris-load-file-sync #'idris-load-file-sync-stub)
      (advice-remove 'idris-eval #'idris-eval-stub))))

(ert-deftest idris-test-idris-filename-to-load ()
  "Test that `idris-filename-to-load' returns expected data structure."
  (cl-flet ((idris-ipkg-find-src-dir-stub () src-dir)
            (idris-find-file-upwards-stub (_ex) ipkg-files)
            (buffer-file-name-stub () "/some/path/to/idris-project/src/Component/Foo.idr"))
    (advice-add 'idris-ipkg-find-src-dir :override #'idris-ipkg-find-src-dir-stub)
    (advice-add 'idris-find-file-upwards :override #'idris-find-file-upwards-stub)
    (advice-add 'buffer-file-name :override #'buffer-file-name-stub)
    (let* ((default-directory "/some/path/to/idris-project/src/Component")
           ipkg-files
           src-dir)
      (unwind-protect
          (progn
            (let ((result (idris-filename-to-load)))
              (should (equal default-directory (car result)))
              (should (equal "Foo.idr" (cdr result))))

            ;; When ipkg sourcedir value is set
            ;; Then return combination of source directory
            ;; and relative path of the file to the source directory
            (let* ((src-dir "/some/path/to/idris-project/src")
                   (result (idris-filename-to-load)))
              (should (equal src-dir (car result)))
              (should (equal "Component/Foo.idr" (cdr result))))

            ;; When ipkg sourcedir value is set
            ;; and idris-protocol-version is greater than 1
            ;; Then return combination of work directory
            ;; (Directory containing the first found ipkg file)
            ;; and relative path of the file to the work directory
            (let* ((ipkg-files '("/some/path/to/idris-project/baz.ipkg"))
                   (idris-protocol-version 2)
                   (result (idris-filename-to-load)))
              (should (equal "/some/path/to/idris-project" (car result)))
              (should (equal "src/Component/Foo.idr" (cdr result)))))

        (advice-remove 'idris-ipkg-find-src-dir #'idris-ipkg-find-src-dir-stub)
        (advice-remove 'idris-find-file-upwards #'idris-find-file-upwards-stub)
        (advice-remove 'buffer-file-name #'buffer-file-name-stub)))))

;; Tests by Yasuhiko Watanabe
;; https://github.com/idris-hackers/idris-mode/pull/537/files
(idris-ert-command-action "test-data/CaseSplit.idr" idris-case-split idris-test-eq-buffer)
(idris-ert-command-action "test-data/MakeLemma.idr" idris-make-lemma idris-test-eq-buffer)
(when (string-match-p "idris2$" idris-interpreter-path)
  (idris-ert-command-action "test-data/GenerateDef.idr" idris-generate-def idris-test-eq-buffer))
(idris-ert-command-action2 "test-data/AddClause.idr" idris-add-clause idris-test-eq-buffer)

(provide 'idris-commands-test)
;;; idris-commands-test.el ends here

;;; idris-xref-test.el --- Tests for Idris Xref backend  -*- lexical-binding: t -*-
;; Copyright (C) 2022  Marek L.

;; Author: Marek L <nospam.keram@gmail.com>
;; Keywords: languages, Idris, Xref, Ert

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

;;; Code:

(require 'ert)
(require 'idris-xref)
(eval-when-compile (require 'cl-lib))

(ert-deftest idris-xref-backend-definitions--error-when-no-connection ()
  "Test that the file is loaded before making search for definition."
  (let ((buffer (find-file-noselect "test-data/AddClause.idr"))
        error-msg)
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward ": Test")
      (condition-case err
          (funcall-interactively 'xref-find-definitions "Test")
        (error (setq error-msg (error-message-string err)))))

    (should (string-match-p "Buffer AddClause.idr has no process" error-msg))

    ;; Cleanup
    (kill-buffer buffer)))

(ert-deftest idris-xref-backend-definitions--not-supported-on-Idris-1 ()
  "Test that user error raised when invoking `xref-find-definitions' used on Idris1."
  (let ((buffer (find-file-noselect "test-data/AddClause.idr"))
        (stub-err-msg "did not understand (synchronous Idris evaluation failed)")
        error-msg)

    (cl-flet ((idris-load-file-sync-stub () nil)
              (idris-eval-stub (&optional &rest _args)
                               (user-error stub-err-msg)))
      (advice-add 'idris-load-file-sync :override #'idris-load-file-sync-stub)
      (advice-add 'idris-eval :override #'idris-eval-stub)

      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward ": Test")

            (condition-case err
                (funcall-interactively 'xref-find-definitions "Test")
              (error (setq error-msg (error-message-string err)))))

        (advice-remove 'idris-load-file-sync #'idris-load-file-sync-stub)
        (advice-remove 'idris-eval #'idris-eval-stub)))

    (should (string-match-p stub-err-msg error-msg))

    ;; Cleanup
    (kill-buffer buffer)))

(ert-deftest idris-xref-backend-definitions--no-results ()
  "Test that an user error message is displayed when no definition found."
  ;; Arrange
  (let ((buffer (find-file-noselect "test-data/AddClause.idr"))
        (idris-protocol-version 3)
        error-msg)

    (cl-flet ((idris-load-file-sync-stub () nil)
              (idris-eval-stub (&optional &rest _args) '()))
      (advice-add 'idris-load-file-sync :override #'idris-load-file-sync-stub)
      (advice-add 'idris-eval :override #'idris-eval-stub)

      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward ": Test")

            (condition-case err
                (funcall-interactively 'xref-find-definitions "Test")
              (user-error (setq error-msg (error-message-string err)))))

        (advice-remove 'idris-load-file-sync #'idris-load-file-sync-stub)
        (advice-remove 'idris-eval #'idris-eval-stub)))

    (should (string-match-p "No definitions found for: Test" error-msg))

    ;; Cleanup
    (kill-buffer buffer)))

(ert-deftest idris-xref-backend-definitions--one-existing-file-result ()
  "Test that point jumps to location in file from result."
  (let* ((buffer (find-file-noselect "test-data/AddClause.idr"))
         (idris-protocol-version 3)
         (eval-result `((("AddClause.Test"
                          (:filename ,(buffer-file-name buffer))
                          (:start 2 0)
                          (:end 2 17))))))

    (cl-flet ((idris-load-file-sync-stub () nil)
              (idris-eval-stub (&optional &rest _args) eval-result))
      (advice-add 'idris-load-file-sync :override #'idris-load-file-sync-stub)
      (advice-add 'idris-eval :override #'idris-eval-stub)

      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward ": Test")
            (should (eq 6 (line-number-at-pos (point))))

            (funcall-interactively 'xref-find-definitions "Test")

            (should (eq 3 (line-number-at-pos (point)))))

        (advice-remove 'idris-load-file-sync #'idris-load-file-sync-stub)
        (advice-remove 'idris-eval #'idris-eval-stub)))

    ;; Cleanup
    (kill-buffer buffer)))

(ert-deftest idris-xref-backend-definitions--one-no-real-file-result ()
  "Test that the term and filename as message is displayed."
  (let* ((buffer (find-file-noselect "test-data/AddClause.idr"))
         (idris-protocol-version 3)
         (eval-result `((("prim__lte_Bits64"
                          (:filename "(Interactive)")
                          (:start 0 0)
                          (:end 0 0)))))
         error-msg)

    (cl-flet ((idris-load-file-sync-stub () nil)
              (idris-eval-stub (&optional &rest _args) eval-result))
      (advice-add 'idris-load-file-sync :override #'idris-load-file-sync-stub)
      (advice-add 'idris-eval :override #'idris-eval-stub)

      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward ": Test")

            (condition-case err
                (funcall-interactively 'xref-find-definitions "Test")
              (user-error (setq error-msg (error-message-string err)))))

        (advice-remove 'idris-load-file-sync #'idris-load-file-sync-stub)
        (advice-remove 'idris-eval #'idris-eval-stub)))

    (should (string-match-p "prim__lte_Bits64 : (Interactive)" error-msg))

    ;; Cleanup
    (kill-buffer buffer)))

(ert-deftest idris-xref-backend-definitions--multiple-results ()
  "Test that results are listed in *xref* buffer."
  (let* ((buffer (find-file-noselect "test-data/AddClause.idr"))
         (idris-protocol-version 3)
         (eval-result `((("prim__lte_Bits64"
                          (:filename "(Interactive)")
                          (:start 0 0)
                          (:end 0 0))
                         ("Prelude.Num.(-)"
                          (:filename "(File-Not-Found)")
                          (:start 30 2)
                          (:end 30 5))
                         ("AddClause.(-)"
                          (:filename "AddClause.idr")
                          (:start 10 0)
                          (:end 10 9))))))
    (cl-flet ((idris-load-file-sync-stub () nil)
              (idris-eval-stub (&optional &rest _args) eval-result))
      (advice-add 'idris-load-file-sync :override #'idris-load-file-sync-stub)
      (advice-add 'idris-eval :override #'idris-eval-stub)

      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward ": Test")

            (funcall-interactively 'xref-find-definitions "Test"))

        (advice-remove 'idris-load-file-sync #'idris-load-file-sync-stub)
        (advice-remove 'idris-eval #'idris-eval-stub)))

    (with-current-buffer "*xref*"
      ;; Assert
      (let ((str (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "AddClause.(-)" str))
        (should (string-match-p "11:" str))
        (should (string-match-p "AddClause.idr" str))
        (should (string-match-p "Prelude.Num.(-)" str))
        (should (string-match-p "prim__lte_Bits64" str)))
      (kill-buffer))

    ;; Cleanup
    (kill-buffer buffer)))

(ert-deftest idris-xref-normalise ()
  "Test normalisation of results from Idris compiler.
The updated candidate should have absolute path to file when possible
and coordinates indexed as expected by Emacs."
  (let* ((buffer (find-file-noselect "test-data/AddClause.idr"))
         (abs-buffer-file-path (buffer-file-name buffer)))
    (kill-buffer buffer)

    (let* ((candidate `("AddClause.Test"
                        (:filename ,abs-buffer-file-path)
                        (:start 2 0)
                        (:end 2 17)))
           (result (idris-xref-normalise candidate)))

      (pcase-let ((`(,_term (:filename ,fn)
                            (:start ,start-line ,start-col)
                            (:end ,_end-line ,_end-col))
                   result))
        (should (string= fn abs-buffer-file-path))
        (should (eq start-line 3))
        (should (eq start-col 0))))

    ;; Test that the filepath is reconstructed from term
    ;; and Idris process current working directory
    (let* ((candidate `("AddClause.Test"
                        (:filename "(File-Not-Found)")
                        (:start 2 0)
                        (:end 2 17)))
           (idris-process-current-working-directory (file-name-directory abs-buffer-file-path))
           (result (idris-xref-normalise candidate)))

      (pcase-let ((`(,_term (:filename ,fn)
                            (:start ,start-line ,start-col)
                            (:end ,_end-line ,_end-col))
                   result))
        (should (string= fn abs-buffer-file-path))
        (should (eq start-line 3))
        (should (eq start-col 0))))

    ;; Test that the original filename returned if no success to
    ;; reconstruct real absolute file path
    (let* ((candidate `("AddClause.Test"
                        (:filename "(File-Not-Found)")
                        (:start 2 0)
                        (:end 2 17)))
           (idris-process-current-working-directory nil)
           (result (idris-xref-normalise candidate)))

      (pcase-let ((`(,_term (:filename ,fn)
                            (:start ,start-line ,start-col)
                            (:end ,_end-line ,_end-col))
                   result))
        (should (string= fn "(File-Not-Found)"))
        (should (eq start-line 3))
        (should (eq start-col 0))))

    ;; Test that the filepath is reconstructed from term
    ;; and (idris-xref-idris-source-directories)
    (let* ((candidate `("AddClause.Test"
                        (:filename "(File-Not-Found)")
                        (:start 2 0)
                        (:end 2 17)))
           (idris-process-current-working-directory nil))
      (cl-flet ((idris-xref-idris-source-directories-stub
                 ()
                 (cons (file-name-directory abs-buffer-file-path) '())))
        (advice-add 'idris-xref-idris-source-directories
                    :override #'idris-xref-idris-source-directories-stub)

        (let ((result (idris-xref-normalise candidate)))
          (pcase-let ((`(,_term (:filename ,fn)
                                (:start ,start-line ,start-col)
                                (:end ,_end-line ,_end-col))
                       result))

            (should (string= fn abs-buffer-file-path))
            (should (eq start-line 3))
            (should (eq start-col 0))))
        (advice-remove 'idris-xref-idris-source-directories
                       #'idris-xref-idris-source-directories-stub)))

    ;; Test that the filepath is reconstructed from term
    ;; and path from idris-xref-idris-source-locations
    (let* ((candidate `("AddClause.Test"
                        (:filename "(File-Not-Found)")
                        (:start 2 0)
                        (:end 2 17)))
           (idris-xref-idris-source-locations (cons (file-name-directory abs-buffer-file-path) '()))
           (result (idris-xref-normalise candidate)))

      (pcase-let ((`(,_term (:filename ,fn)
                            (:start ,start-line ,start-col)
                            (:end ,_end-line ,_end-col))
                   result))
        (should (string= fn abs-buffer-file-path))
        (should (eq start-line 3))
        (should (eq start-col 0))))))

(provide 'idris-xref-test)
;;; idris-xref-test.el ends here

(require 'idris-mode)
(require 'idris-commands)
(require 'inferior-idris)
(require 'idris-ipkg-mode)
(require 'cl-lib)
(require 'idris-test-utils)



;(idris-test-text-update-command "test-data/CaseSplit.idr" idris-case-split '(lambda (x y) 'eq))
(ert-deftest idris-test-idris-run ()
  (let ((buffer (find-file "test-data/Empty.idr")))
    (should buffer)
    (with-current-buffer buffer
      (idris-run)
      (dotimes (_ 5) (accept-process-output nil 0.1))
      (should idris-process)
      )
    (kill-buffer))
  (idris-quit))


(ert-deftest idris-test-idris-connect-after-idris-run ()
  (let ((buffer (find-file "test-data/Empty.idr")))
    (with-current-buffer buffer
      (idris-load-file)
      (dotimes (_ 5) (accept-process-output nil 0.1))
      (should idris-connection)
    (kill-buffer)))
  (idris-quit))

(idris-ert-command-action "test-data/CaseSplit.idr" idris-case-split idris-test-eq-buffer)
(idris-ert-command-action "test-data/MakeLemma.idr" idris-make-lemma idris-test-eq-buffer)
(idris-ert-command-action "test-data/GenerateDef.idr" idris-generate-def idris-test-eq-buffer)
(idris-ert-command-action2 "test-data/AddClause.idr"
			  idris-add-clause
			  idris-test-eq-buffer)


(null nil)
(provide 'idris-tests3)
;;; idris-tests.el ends here

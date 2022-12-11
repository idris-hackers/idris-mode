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

;;; Code:

(require 'idris-mode)
(require 'idris-navigate)
(require 'inferior-idris)
(require 'idris-ipkg-mode)
(require 'cl-lib)
(require 'idris-test-utils)


(ert-deftest trivial-test ()
  (should t))

(ert-deftest idris-test-idris-editor-port ()
  (let ((output "Can't find import Prelude\n37072\n"))
    (should (string-match idris-process-port-output-regexp output))
    (should (string= "Can't find import Prelude\n" (match-string 1 output)))
    (should (string= "37072" (match-string 2 output))))
  (let ((output "37072\n"))
    (should (string-match idris-process-port-output-regexp output))
    (should (null (match-string 1 output)))
    (should (string= "37072" (match-string 2 output)))))

(ert-deftest idris-test-idris-quit ()
  "Ensure that running Idris and quitting doesn't leave behind
unwanted buffers."
  (let ((before (buffer-list)))
    (idris-repl)
    (dotimes (_ 5) (accept-process-output nil 1))
    (idris-quit)
    (let* ((after (buffer-list))
           (extra (cl-set-difference after before)))
      (should (= (length extra) 0)))))

(ert-deftest idris-test-idris-quit-logging-enabled ()
  "Ensure that running Idris and quitting doesn't leave behind
unwanted buffers. In particular, only *idris-events* should
remain."
  (let ((before (buffer-list))
        (idris-log-events 't))
    (idris-repl)
    (dotimes (_ 5) (accept-process-output nil 1))
    (idris-quit)
    (let* ((after (buffer-list))
           (extra (cl-set-difference after before)))
      (should (= (length extra) 1))
      (should (string= (buffer-name (car extra)) idris-event-buffer-name)))

    ;; Cleanup
    (kill-buffer idris-event-buffer-name)))

(ert-deftest idris-test-hole-load ()
  "Test the hole-list-on-load setting."
  ;;; The default setting should be to show holes
  (should idris-hole-show-on-load)

  (let ((buffer (find-file "test-data/MetavarTest.idr")))
    ;;; Check that the file was loaded
    (should (bufferp buffer))

    ;;; Check that it shows the hole list with the option turned on
    (with-current-buffer buffer
      (idris-load-file))
    ;;; Allow async stuff to happen
    (dotimes (_ 5) (accept-process-output nil 1))
    (let ((mv-buffer (get-buffer idris-hole-list-buffer-name)))
      ;; The buffer exists and contains characters
      (should (bufferp mv-buffer))
      (should (> (buffer-size mv-buffer) 10)))
    (idris-quit)

    ;; Now check that it works with the setting the other way
    (let ((idris-hole-show-on-load nil))
      (with-current-buffer buffer
        (idris-load-file))
      (dotimes (_ 5) (accept-process-output nil 1))
      (let ((mv-buffer (get-buffer idris-hole-list-buffer-name)))
        (should-not (bufferp mv-buffer))
        (should (null mv-buffer))))
    ;; Clean up
    (kill-buffer))

  ;; More cleanup
  (idris-quit))

(ert-deftest idris-test-proof-search ()
  "Test that proof search works"
  :expected-result (if (string-match-p "idris2" idris-interpreter-path)
                       :failed
                     :passed)
  (let ((buffer (find-file "test-data/ProofSearch.idr")))
    (with-current-buffer buffer
      (idris-load-file)
      (dotimes (_ 5) (accept-process-output nil 1))
      (goto-char (point-min))
      (re-search-forward "search_here")
      (goto-char (match-beginning 0))
      (idris-proof-search)
      (dotimes (_ 5) (accept-process-output nil 1))
      (should (looking-at-p "lteSucc (lteSucc (lteSucc (lteSucc (lteSucc lteZero))))"))
      (move-beginning-of-line nil)
      (delete-region (point) (line-end-position))
      (insert "prf = ?search_here")
      (save-buffer)
      (kill-buffer)))

  ;; More cleanup
  (idris-quit))

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
      (dotimes (_ 5) (accept-process-output nil 1))
      (should (get-buffer idris-notes-buffer-name)))
    (with-current-buffer (get-buffer idris-notes-buffer-name)
      (goto-char (point-min))
      (should (re-search-forward "Nat" nil t))) ;; check that the buffer has something error-like
    (with-current-buffer buffer
      (kill-buffer))
    (idris-quit)))

(ert-deftest idris-test-idris-type-search ()
  "Test that `idris-type-search' produces output in Idris info buffer."
  (idris-run)
  (funcall-interactively 'idris-type-search "Nat")
  (with-current-buffer (get-buffer idris-info-buffer-name)
    (goto-char (point-min))
    (should (re-search-forward "Zero" nil t)))
 (idris-quit))

(ert-deftest idris-test-ipkg-packages-with-underscores-and-dashes ()
  "Test that loading an ipkg file can have dependencies on packages with _ or - in the name."
  (let ((buffer (find-file "test-data/package-test/Packaging.idr")))
    (with-current-buffer buffer
      (should (equal '("-p" "idris-free" "-p" "recursion_schemes")
                     (idris-ipkg-pkgs-flags-for-current-buffer)))
      (kill-buffer buffer))
    (idris-quit)))

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
    (idris-delete-ibc t)
    ;; Cleanup
    ;; (sit-for 3) ;; usefull for manual inspection before restore
    (erase-buffer)
    (insert buffer-content)
    (save-buffer)
    (kill-buffer)
    (idris-quit)))

(ert-deftest idris-backard-toplevel-navigation-test-2pTac9 ()
  "Test idris-backard-toplevel navigation command."
  (idris-test-with-temp-buffer
   "interface DataStore (m : Type -> Type) where
  data Store : Access -> Type

  connect : ST m Var [add (Store LoggedOut)]
  disconnect : (store : Var) -> ST m () [remove store (Store LoggedOut)]

  readSecret : (store : Var) -> ST m String [store ::: Store LoggedIn]
  login : (store : Var) ->
          ST m LoginResult [store ::: Store LoggedOut :->
                             (\\res => Store (case res of
                                                  OK => LoggedIn
                                                  BadPassword => LoggedOut))]
  logout : (store : Var) ->
           ST m () [store ::: Store LoggedIn :-> Store LoggedOut]

getData : (ConsoleIO m, DataStore m) =>
          (failcount : Var) -> ST m () [failcount ::: State Integer]
getData failcount
   = do st <- call connect
        OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               disconnect st
                               getData failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        disconnect st
        getData failcount

getData2 : (ConsoleIO m, DataStore m) =>
           (st, failcount : Var) ->
           ST m () [st ::: Store {m} LoggedOut, failcount ::: State Integer]
getData2 st failcount
   = do OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               getData2 st failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        getData2 st failcount"
   (goto-char (point-max))
   (idris-backward-toplevel)
   (should (looking-at "getData2 st"))
   ;; (goto-char (point-max))
   (search-backward "Number")
   (idris-backward-toplevel)
   (should (looking-at "getData failcount"))
   (search-backward "LoggedIn")
   (idris-backward-toplevel)
   (should (looking-at "interface DataStore"))
   ))

(ert-deftest idris-forward-toplevel-navigation-test-2pTac9 ()
  "Test idris-forard-toplevel navigation command."
  (idris-test-with-temp-buffer-point-min
   "interface DataStore (m : Type -> Type) where
  data Store : Access -> Type

  connect : ST m Var [add (Store LoggedOut)]
  disconnect : (store : Var) -> ST m () [remove store (Store LoggedOut)]

  readSecret : (store : Var) -> ST m String [store ::: Store LoggedIn]
  login : (store : Var) ->
          ST m LoginResult [store ::: Store LoggedOut :->
                             (\\res => Store (case res of
                                                  OK => LoggedIn
                                                  BadPassword => LoggedOut))]
  logout : (store : Var) ->
           ST m () [store ::: Store LoggedIn :-> Store LoggedOut]

getData : (ConsoleIO m, DataStore m) =>
          (failcount : Var) -> ST m () [failcount ::: State Integer]
getData failcount
   = do st <- call connect
        OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               disconnect st
                               getData failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        disconnect st
        getData failcount

getData2 : (ConsoleIO m, DataStore m) =>
           (st, failcount : Var) ->
           ST m () [st ::: Store {m} LoggedOut, failcount ::: State Integer]
getData2 st failcount
   = do OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               getData2 st failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        getData2 st failcount"
   (search-forward "DataStore")
   (idris-forward-toplevel)
   (should (empty-line-p))
   (skip-chars-backward " \t\r\n\f")
   (should (looking-back "Store LoggedOut]" (line-beginning-position)))
   (idris-forward-toplevel)
   (should (looking-at "getData failcount"))
   (idris-forward-toplevel)
   (should (empty-line-p))
   (skip-chars-backward " \t\r\n\f")
   (should (looking-back "getData failcount" (line-beginning-position)))
   ;; (goto-char (point-max))
   (search-forward "Number")
   (idris-forward-toplevel)
   (should (looking-back "getData2 st failcount" (line-beginning-position)))
   ))

(ert-deftest idris-backard-statement-navigation-test-2pTac9 ()
  "Test idris-backard-statement navigation command."
  (idris-test-with-temp-buffer
   "interface DataStore (m : Type -> Type) where
  data Store : Access -> Type

  connect : ST m Var [add (Store LoggedOut)]
  disconnect : (store : Var) -> ST m () [remove store (Store LoggedOut)]

  readSecret : (store : Var) -> ST m String [store ::: Store LoggedIn]
  login : (store : Var) ->
          ST m LoginResult [store ::: Store LoggedOut :->
                             (\\res => Store (case res of
                                                  OK => LoggedIn
                                                  BadPassword => LoggedOut))]
  logout : (store : Var) ->
           ST m () [store ::: Store LoggedIn :-> Store LoggedOut]

getData : (ConsoleIO m, DataStore m) =>
          (failcount : Var) -> ST m () [failcount ::: State Integer]
getData failcount
   = do st <- call connect
        OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               disconnect st
                               getData failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        disconnect st
        getData failcount

getData2 : (ConsoleIO m, DataStore m) =>
           (st, failcount : Var) ->
           ST m () [st ::: Store {m} LoggedOut, failcount ::: State Integer]
getData2 st failcount
   = do OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               getData2 st failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        getData2 st failcount"
   (goto-char (point-max))
   (idris-backward-statement)
   (should (looking-at "getData2 st"))
   (search-backward "Number")
   (idris-backward-statement)
   (should (looking-at "putStrLn ("))
   (idris-backward-statement)
   (should (looking-at "write failcount"))
   (search-backward "BadPassword")
   (idris-backward-statement)
   (should (looking-at "| BadPassword"))
   (idris-backward-statement)
   (should (looking-at "= do OK"))
   (idris-backward-statement)
   (should (looking-at "getData2 st"))
   (idris-backward-statement)
   (should (looking-at "ST m ()"))
   ))

(ert-deftest idris-forward-statement-navigation-test-2pTac9 ()
  "Test idris-forard-statement navigation command."
  (idris-test-with-temp-buffer-point-min
   "interface DataStore (m : Type -> Type) where
  data Store : Access -> Type

  connect : ST m Var [add (Store LoggedOut)]
  disconnect : (store : Var) -> ST m () [remove store (Store LoggedOut)]

  readSecret : (store : Var) -> ST m String [store ::: Store LoggedIn]
  login : (store : Var) ->
          ST m LoginResult [store ::: Store LoggedOut :->
                             (\\res => Store (case res of
                                                  OK => LoggedIn
                                                  BadPassword => LoggedOut))]
  logout : (store : Var) ->
           ST m () [store ::: Store LoggedIn :-> Store LoggedOut]

getData : (ConsoleIO m, DataStore m) =>
          (failcount : Var) -> ST m () [failcount ::: State Integer]
getData failcount
   = do st <- call connect
        OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               disconnect st
                               getData failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        disconnect st
        getData failcount

getData2 : (ConsoleIO m, DataStore m) =>
           (st, failcount : Var) ->
           ST m () [st ::: Store {m} LoggedOut, failcount ::: State Integer]
getData2 st failcount
   = do OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               getData2 st failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        getData2 st failcount"
   (search-forward "DataStore")
   (idris-forward-statement)
   (should (looking-back "where" (line-beginning-position)))
   (idris-forward-statement)
   (should (looking-back "Access -> Type" (line-beginning-position)))
   (idris-forward-statement)
   (should (looking-back "Store LoggedOut)]" (line-beginning-position)))
   ))

(provide 'idris-tests)
;;; idris-tests.el ends here

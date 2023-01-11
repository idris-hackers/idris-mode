;;; idris-navigate-test.el --- Tests for idris-navigate

(require 'idris-mode)
(require 'idris-navigate)

(require 'ert)
(require 'idris-test-utils)

;;; Code:

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

(provide 'idris-navigate-test)
;;; idris-navigate-test.el ends here

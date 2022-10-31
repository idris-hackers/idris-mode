;;; flycheck-idris.el --- Major mode for editing Idris code -*- lexical-binding: t -*-

;; Copyright (C) 2022

;; Author:
;; URL: https://github.com/idris-hackers/idris-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24") (prop-menu "0.1") (cl-lib "0.5"))
;; Version: 1.1.0


;;; Commentary:

;; FlyCheck checkers for Idris(2)

;;; Code:

(require 'flycheck)
(require 'idris-mode)

(flycheck-define-checker idris
  "An Idris syntax and type checker."
  :command ("idris"
            "--check" "--nocolor" "--warnpartial"
            ;; Compute the command-line options similarly to inferior-idris
            (eval (idris-compute-flags))
            source)
  :error-patterns
  ((warning line-start
            (file-name)
            ":"
            line
            ":"
            column
            ":Warning - "
            (message (and (* nonl) (* "\n" (not (any "/" "~")) (* nonl)))))
   (error line-start
          (file-name)
          ":"
          line
          ":"
          column
          ":"
          (message (and (* nonl) (* "\n" (not (any "/" "~")) (* nonl))))))
  :modes idris-mode)


(flycheck-define-checker idris2
  "An Idris2 syntax and type checker."
  :command ("idris2"
            "--check" "--no-colour"
            ;; Compute the command-line options similarly to inferior-idris
            (eval (idris-compute-flags))
            source)
  :error-patterns ((warning line-start
                            "Warning: "
                            (message (seq (and (* nonl)
                                               (* "\n"
                                                  (not (any "/" "~"))
                                                  (* nonl)))
                                          (+ "\n")))
                            (file-name)
                            ":"  line
                            ":"  column
                            "--" end-line
                            ":"  end-column
                            )
                   (error line-start
                          "Error: "
                          (message (seq (and (* nonl)
                                             (* "\n"
                                                (not (any "/" "~"))
                                                (* nonl)))
                                        (+ "\n")))
                          (file-name)
                          ":"  line
                          ":"  column
                          "--" end-line
                          ":"  end-column
                          )
                   )
  :modes idris-mode)

(setq flycheck-idris2-executable "idris2")

;;; ###autoload
(add-to-list 'flycheck-checkers 'idris)
(add-to-list 'flycheck-checkers 'idris2)


(provide 'flycheck-idris)
;;; flycheck-idris.el ends here

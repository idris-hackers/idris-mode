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
            source-original)
  :error-patterns
  ((warning line-start
            (file-name)
            ":"
            line
            ":"
            column
            "-"
            end-column
            ":" line-end "\n"
            (one-or-more blank) "|\n"
            (one-or-more digit) (one-or-more blank) "|" (one-or-more not-newline) "\n"
            (one-or-more blank) "|" (zero-or-more blank) (one-or-more "~") "\n"
            "Warning - "(message (one-or-more not-newline)
                                 (zero-or-more "\n" (one-or-more not-newline))))
   (error line-start
          (file-name)
          ":"
          line
          ":"
          column
          "-"
          end-column
          ":" line-end "\n"
          (one-or-more blank) "|\n"
          (one-or-more digit) (one-or-more blank) "|" (one-or-more not-newline) "\n"
          (one-or-more blank) "|" (zero-or-more blank) (one-or-more "~") "\n"
          (one-or-more not-newline) "\n"
          (one-or-more blank) (one-or-more not-newline) "\n\n"
          (message (one-or-more not-newline)
                   (zero-or-more "\n" (one-or-more not-newline)))))
  :error-filter delete-dups
  :modes idris-mode)


(flycheck-define-checker idris2
  "An Idris2 syntax and type checker."
  :command ("idris2"
            "--check" "--no-colour"
            ;; Compute the command-line options similarly to inferior-idris
            (eval (idris-compute-flags))
            source-original)
  :error-patterns ((warning line-start
                            "Warning: "
                            (message (one-or-more not-newline)
                                     (zero-or-more "\n" (one-or-more not-newline))
                                     "\n\n")
                            (one-or-more (not ":")) ;; (file-name)
                            ":"  line
                            ":"  column
                            "--" end-line
                            ":"  end-column)
                   (error line-start
                          (zero-or-one "Uncaught error: ")
                          "Error: "
                          (zero-or-one "While processing" (one-or-more (not ".")) ".")
                          (message (one-or-more not-newline)
                                   (zero-or-more "\n" (one-or-more not-newline))
                                   "\n\n")
                          (one-or-more (not ":")) ;; (file-name)
                          ":"  line
                          ":"  column
                          "--" end-line
                          ":"  end-column))
  :modes idris-mode)


;;; ###autoload
(add-to-list 'flycheck-checkers 'idris)
(add-to-list 'flycheck-checkers 'idris2)


(provide 'flycheck-idris)
;;; flycheck-idris.el ends here

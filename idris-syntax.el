;; idris-syntax.el --- idris syntax highlighting -*- lexical-binding: t -*-
;;
;; Copyright (C) 2013 tim dixon, David Raymond Christiansen and Hannes Mehnert
;;
;; Authors: tim dixon <tdixon51793@gmail.com>,
;;          David Raymond Christiansen <drc@itu.dk>,
;;          Hannes Mehnert <hame@itu.dk>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'idris-core)
(require 'idris-common-utils)
(require 'cl-lib)

(defgroup idris-faces nil "Idris highlighting" :prefix 'idris :group 'idris)

(defface idris-identifier-face
  '((t (:inherit default)))
  "The face to highlight Idris identifiers with."
  :group 'idris-faces)

(defface idris-metavariable-face
  '((t (:inherit idris-identifier-face)))
  "The face to highlight Idris metavariables with."
  :group 'idris-faces)

(defface idris-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "The face to highlight Idris keywords with."
  :group 'idris-faces)

(defface idris-module-face
  '((t (:inherit font-lock-variable-name-face)))
  "The face to highlight Idris module names with."
  :group 'idris-faces)

(defface idris-directive-face
  '((t (:inherit font-lock-keyword-face)))
  "The face to highlight Idris compiler directives."
  :group 'idris-faces)

(defface idris-directive-argument-face
  '((t (:inherit font-lock-preprocessor-face)))
  "The face to highlight arguments to Idris directives."
  :group 'idris-faces)

(defface idris-definition-face
  '((t (:inherit font-lock-function-name-face)))
  "The face to highlight things being defined in."
  :group 'idris-faces)

(defface idris-parameter-face
  '((t (:inherit font-lock-constant-face)))
  "The face to highlight formal parameters to function definitions with."
  :group 'idris-faces)

(defface idris-colon-face
  '((t (:inherit font-lock-variable-name-face)))
  "The face to highlight ':' in type annotations with."
  :group 'idris-faces)

(defface idris-equals-face
  '((t (:inherit font-lock-variable-name-face)))
  "The face to highlight '=' in definitions with."
  :group 'idris-faces)

(defface idris-operator-face
  '((t (:inherit font-lock-variable-name-face)))
  "The face to highlight operators with."
  :group 'idris-faces)

(defface idris-char-face
  '((t (:inherit font-lock-string-face)))
  "The face used to highlight character literals in Idris"
  :group 'idris-faces)

(defface idris-unsafe-face
  '((t (:inherit font-lock-warning-face)))
  "The face used to highlight unsafe Idris features, such as %assert_total"
  :group 'idris-faces)


(defvar idris-definition-keywords
  '("data" "class" "codata" "record" "postulate")
  "Keywords that introduce some identifier.")

(defvar idris-operator-regexp
  (let ((op "-!#$%&*+./<=>@\\\\^|~:"))
    (concat "\\?[" op "]+" ; starts with ?, has at least one more opchar
            "\\|" "["op"][" op "?]*")) ; doesn't start with ?
  "A regular expression matching an Idris operator.")

(defconst idris-syntax-table
  (let ((st (make-syntax-table)))

    ;; Matching parens
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)

    ;; Matching {}, but with nested comments
    (modify-syntax-entry ?\{ "(} 1bn" st)
    (modify-syntax-entry ?\} "){ 4bn" st)
    (modify-syntax-entry ?\- "_ 123" st)
    (modify-syntax-entry ?\n ">" st)

    ;; ' and _ can be names
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?_ "w" st)


    ;; Idris operator chars
    (mapc #'(lambda (ch) (modify-syntax-entry ch "_" st))
          "!#$%&*+./<=>@^|~:")

    ;; Whitespace is whitespace
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)

    ;; ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "/" st)

    st))

(defconst idris-keywords
  '("abstract" "case" "covering" "default" "do" "dsl" "else" "if"
    "implicit" "import" "in" "infix" "infixl" "infixr" "instance"
    "module" "mutual" "namespace" "of" "let" "parameters" "partial"
    "pattern" "prefix" "private" "proof" "public" "rewrite" "syntax"
    "tactics" "then" "total" "using" "where" "with"))


(defconst idris-special-char-regexp
  (let ((idris-special-chars
         (cl-loop for code
                  across "0abfnrtv\"'\\"
                  collecting (concat "'\\" (string code) "'")))
        (idris-ascii-escapes
         (cl-loop for esc
                  in '("NUL" "SOH" "STX" "ETX" "EOT" "ENQ"
                       "ACK" "BEL" "BS" "HT" "LF" "VT" "FF"
                       "CR" "SO" "SI" "DLE" "DC1" "DC2"
                       "DC3" "DC4" "NAK" "SYN" "ETB" "CAN"
                       "EM" "SUB" "ESC" "FS" "GS" "RS" "US"
                       "SP" "DEL")
                  collecting (concat "'\\" esc "'"))))
    (concat "\\(?:'\"'\\)"
            "\\|"
            "\\(?:'\\\\[0-9]+'\\)"
            "\\|"
            "\\(?:'\\\\o[0-7]+'\\)"
            "\\|"
            "\\(?:'\\\\x[0-9a-fA-F]+'\\)"
            "\\|"
            "\\(?:'[^'\\]'\\)"
            "\\|"
            (regexp-opt (append idris-ascii-escapes idris-special-chars)))))

(defun idris-syntax-propertize-function (begin end)
  "Add syntax properties to a region of the buffer that the
syntax table won't support, such as characters."
  (save-excursion
    (goto-char begin)
    (while (re-search-forward idris-special-char-regexp end t)
      (let ((open (match-beginning 0))
            (close (match-end 0)))
        (add-text-properties open (1+ open) '(syntax-table (7 . ?\')))
        (add-text-properties (1- close) close '(syntax-table (7 . ?\')))))
    ;; Backslash \ is not escaping in \(x, y) -> x + y.
    (goto-char begin)
    (while (re-search-forward "\\\\(" end t)
      (let ((open (match-beginning 0)))
        (add-text-properties open (1+ open) '(syntax-table (1 . nil)))))))

(defconst idris-font-lock-keyword-regexp
  (concat "\\(?:[^a-zA-Z%]\\|^\\)\\("
          (regexp-opt idris-keywords 'words)
          "\\)\\(?:[^a-zA-Z]\\|$\\)")
  "A regexp for matching Idris keywords")

(defun idris-font-lock-literate-search (regexp lidr limit)
  "Find REGEXP in Idris source between point and LIMIT, where LIDR is non-nil for literate files..

See the documentation for search-based fontification,
esp. `font-lock-defaults', for details."
  (if (re-search-forward regexp limit t)
      (if (or (and (not lidr) ;; normal syntax - ensure not in docs
                   (save-excursion
                     (move-beginning-of-line nil)
                     (not (looking-at-p "^\\s-*|||"))))
              (and lidr
                   (save-excursion ;; LIDR syntax - ensure in code, not in docs
                     (move-beginning-of-line nil)
                     (and (looking-at-p "^> ")
                          (not (looking-at-p "^>\\s-*|||"))))))
          t
        (idris-font-lock-literate-search regexp lidr limit))
    nil))

;; This should be a function so that it evaluates `idris-lidr-p' at the correct time
(defun idris-font-lock-defaults ()
  (cl-flet ((line-start (regexp)
                        (if (idris-lidr-p)
                            (concat "^>" regexp)
                          (concat "^" regexp))))
    `('(
        ;; Imports
        (,(line-start "\\(import\\)\\s-+\\(public\\)")
         (1 'idris-keyword-face)
         (2 'idris-keyword-face))
        ;; Documentation comments.
        (,(line-start "\\s-*\\(|||\\)\\(.*\\)$")
         (1 font-lock-comment-delimiter-face)
         (2 font-lock-doc-face))
        (,(line-start "\\s-*\\(|||\\)\\s-*\\(@\\)\\s-*\\(\\sw+\\)")
         (1 font-lock-comment-delimiter-face t)
         (2 font-lock-comment-delimiter-face t)
         (3 'idris-parameter-face t))
        ;; %assert_total
        ("%assert_total" . 'idris-unsafe-face)
        ;; `%access`, `%default`, etc
        (,(line-start "\\s-*\\(%\\w+\\)\\s-*\\(.*\\)")
         (1 'idris-directive-face)
         (2 'idris-directive-argument-face))
        ;; Definitions with keywords.
        (,(format "\\(%s\\) \\(\\w+\\)" (regexp-opt idris-definition-keywords))
         (1 'idris-keyword-face)
         (2 'idris-definition-face))
        ;; Type declarations
        ;; TODO: this won't match, e.g. f:a
        (,(line-start "\\s-*\\(\\w+\\|(\\s_+)\\)\\s-+\\(:\\)\\s-+")
         (1 'idris-definition-face)
         (2 'idris-colon-face))
        (,(line-start "\\s-*\\(total\\|partial\\)\\s-+\\(\\w+\\)\\s-+\\(:\\)\\s-+")
         (1 'idris-keyword-face)
         (2 'idris-definition-face)
         (3 'idris-colon-face))
        ;; "where"-blocks
        (,(line-start "\\s-+\\(where\\)\\s-+\\(\\w+\\)\s-*\\(.?*\\)\\(=\\)")
         (1 'idris-keyword-face)
         (2 'idris-definition-face)
         (3 'idris-parameter-face)
         (4 'idris-equals-face))
        (,(line-start "\\s-+\\(where\\)\\s-+\\(\\w+\\|(\\s_+)\\)\s-*\\(:\\)\\s-*")
         (1 'idris-keyword-face)
         (2 'idris-definition-face)
         (3 'idris-colon-face))
        (,(line-start "\\s-+\\(where\\)\\s-+\\(total\\|partial\\)\\s-+\\(\\w+\\)\s-*\\(:\\)\\s-*")
         (1 'idris-keyword-face)
         (2 'idris-keyword-face)
         (3 'idris-definition-face)
         (4 'idris-colon-face))
        ;; Provisional definitions with ?=
        (,(line-start "\\s-*\\(\\w+\\|(\\s_+)\\)\s-*\\(.*?\\)\\(\\?=\\)\\(\\s-*{\\([a-zA-Z_][a-zA-Z_0-9']*\\)}\\)?")
         (1 'idris-definition-face)
         (2 'idris-parameter-face)
         (3 'idris-equals-face)
         (5 'idris-metavariable-face))
        ;; Vanilla definitions with = (and optionally let ... in ...)
        (,(line-start "\\s-*\\(\\w+\\|(\\s_+)\\)\s-*\\(.*?\\)\\(=\\)[^>]")
         (1 'idris-definition-face)
         (2 'idris-parameter-face)
         (3 'idris-equals-face))
        (,(line-start "\\s-*\\(\\w+\\|(\\s_+)\\)\\s-+\\(.*?\\)\\s-*\\(impossible\\)")
         (1 'idris-definition-face)
         (2 'idris-parameter-face)
         (3 'idris-keyword-face))
        ;; Definitions using "with"
        (,(line-start "\\s-*\\(\\w+\\)\s-*\\(.?*\\)\\(with\\)\\(.?*\\)")
         (1 'idris-definition-face)
         (2 'idris-parameter-face)
         (3 'idris-keyword-face)
         (4 'idris-parameter-face))
        ;; Other keywords
        (,(apply-partially #'idris-font-lock-literate-search idris-font-lock-keyword-regexp (idris-lidr-p))
         (1 'idris-keyword-face))
        ;; Operators
        (,(apply-partially #'idris-font-lock-literate-search idris-operator-regexp (idris-lidr-p)) . 'idris-operator-face)
        ;; Metavariables
        (,(apply-partially #'idris-font-lock-literate-search "\\?[a-zA-Z_]\\w*" (idris-lidr-p)) . 'idris-metavariable-face)
        ;; Identifiers
        (,(apply-partially #'idris-font-lock-literate-search "[a-zA-Z_]\\w*" (idris-lidr-p)) . 'idris-identifier-face)
        ;; Scary stuff
        (,(apply-partially #'idris-font-lock-literate-search
                           (regexp-opt '("believe_me" "really_believe_me" "assert_total" "assert_smaller" "prim__believe_me"))
                           (idris-lidr-p))
         0 'idris-unsafe-face t)
        ;; TODO: operator definitions.
        ;; TODO: let ... in ...
        ))))



(provide 'idris-syntax)

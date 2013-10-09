;; idris-syntax.el - major mode for editing idris source files
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


(defgroup idris-faces nil "Idris highlighting" :prefix 'idris :group 'idris)

(defface idris-identifier-face
  '((t (:inherit default)))
  "The face to highlight idris identifiers with."
  :group 'idris-faces)

(defface idris-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "The face to highlight idris keywords with."
  :group 'idris-faces)

(defface idris-module-face
  '((t (:inherit font-lock-variable-name-face)))
  "The face to highlight module names with."
  :group 'idris-faces)

(defface idris-directive-face
  '((t (:inherit font-lock-keyword-face)))
  "The face to highlight directives."
  :group 'idris-faces)

(defface idris-directive-argument-face
  '((t (:inherit font-lock-preprocessor-face)))
  "The face to highlight arguments to directives."
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

(defvar idris-definition-keywords
  '("data" "class" "codata" "record")
  "Keywords that introduce some identifier.")

(defvar idris-operator-regexp
  "[-!#$%&\*\+./<=>\?@\\^|~:]+"
  "A regular expression matching an Idris operator.")

(defconst idris-syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))

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
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?' "w" st)

    ;; Whitespace is whitespace
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    st))

(defconst idris-keywords
  '("abstract" "attack" "case" "compute" "do" "dsl" "else" "exact" "focus" "if"
    "import" "in" "infix" "infixl" "infixr" "instance" "intros" "module" "mutual"
    "namespace" "of" "let" "parameters" "partial" "pattern" "prefix" "private"
    "public" "refine" "rewrite" "solve" "syntax" "term" "then" "total" "trivial"
    "try" "using" "where" "with"))

(defconst idris-font-lock-defaults
    `('(
         ;; {- Block comments -}
         ("\\({-\\)\\(.*\\)\\(-}\\)"
           (1 font-lock-comment-delimiter-face)
           (2 font-lock-comment-face)
           (3 font-lock-comment-delimiter-face))
         ;; TODO: this doesn't let you do newlines
         ;; Documentation comments.
         ("\\(--\\s-*|\\)\\(.*\\)\\(\n\\(--\\)\\(.*\\)\\)*"
           (1 font-lock-comment-delimiter-face)
           (2 font-lock-doc-face)
           (4 font-lock-comment-delimiter-face)
           (5 font-lock-doc-face))
         ;; Ordinary comments.
         ("\\(--\\)\s*\\(.*\\)"
           (1 font-lock-comment-delimiter-face)
           (2 font-lock-comment-face))
         ;; `%access`, `%default`, etc
         ("^%\\(\\w+\\)\\s-*\\(.+\\)"
           (1 'idris-directive-face)
           (2 'idris-directive-argument-face))
         ;; Definitions with keywords.
         (,(format "\\(%s\\) \\(\\w+\\)" (regexp-opt idris-definition-keywords))
           (1 'idris-keyword-face)
           (2 'idris-definition-face))
         ;; Type annotations.
         ;; TODO: this won't match, e.g. f:a
         ("^\\s-*\\(\\w+\\)\\s-+\\(:\\)\\s-+"
           (1 'idris-definition-face)
           (2 'idris-colon-face))
         ;; Operators
         (,idris-operator-regexp . 'idris-operator-face)
         ;; "where"-blocks
         ("^\\s-+\\(where\\)\\s-+\\(\\w+\\)\s-*\\(.?*\\)\\(=\\)"
           (1 'idris-keyword-face)
           (2 'idris-definition-face)
           (3 'idris-parameter-face)
           (4 'idris-equals-face))
         ("^\\s-+\\(where\\)\\s-+\\(\\w+\\)\s-*\\(:\\)\\s-*"
           (1 'idris-keyword-face)
           (2 'idris-definition-face)
           (3 'idris-colon-face))
         ;; Vanilla definitions with = (and optionally let ... in ...)
         ;; TODO: clean up how parameters are picked up
         ("^\\s-*\\(\\w+\\)\s-*\\(.?*\\)\\(=\\)"
           (1 'idris-definition-face)
           (2 'idris-parameter-face)
           (3 'idris-equals-face))
         ;; Definitions using "with"
         ("^\\s-*\\(\\w+\\)\s-*\\(.?*\\)\\(with\\)\\(.?*\\)"
           (1 'idris-definition-face)
           (2 'idris-parameter-face)
           (3 'idris-keyword-face)
           (4 'idris-parameter-face))
         ;; Character literals
         ("'\\(?:\\(?:[^']\\)\\|\\(?:\\\\[^']+\\)\\)'"
           (0 font-lock-string-face t))
         ;; Other keywords
         (,(regexp-opt idris-keywords 'words) . 'idris-keyword-face)
         ;; Identifiers
         ("[a-zA-Z_]\\w*" . 'idris-identifier-face)
         ;; TODO: operator definitions.
         ;; TODO: let ... in ...
)))



(provide 'idris-syntax)

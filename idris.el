(defvar idris-identifier-face
  'default
  "The face to highlight idris identifiers with.")

(defvar idris-keyword-face
  'font-lock-keyword-face
  "The face to highlight idris keywords with.")

(defvar idris-module-face
  'font-lock-variable-name-face
  "The face to highlight module names with.")

(defvar idris-directive-face
  'font-lock-keyword-face
  "The face to highlight directives.")

(defvar idris-directive-argument-face
  'font-lock-preprocessor-face
  "The face to highlight arguments to directives.")

(defvar idris-definition-face
  'font-lock-function-name-face
  "The face to highlight things being defined in.")

(defvar idris-parameter-face
  'font-lock-constant-face
  "The face to highlight formal parameters to function definitions with.")

(defvar idris-colon-face
  'font-lock-variable-name-face
  "The face to highlight ':' in type annotations with.")

(defvar idris-equals-face
  'font-lock-variable-name-face
  "The face to highlight '=' in definitions with.")

(defvar idris-operator-face
  'font-lock-variable-name-face
  "The face to highlight operators with.")

(defvar idris-definition-keywords
  '("data" "class" "codata" "record")
  "Keywords that introduce some identifier.")

(defvar idris-operator-regexp
  "[-!#$%&\*\+./<=>\?@\\^|~:]+"
  "A regular expression matching an Idris operator.")

(defun idris-load-faces ()
  (interactive)
  (setq font-lock-defaults
    `('(
         ;; {- Block comments -}
         ("\\({-\\)\\(.*\\)\\(-}\\)"
           (1 ,font-lock-comment-delimiter-face)
           (2 ,font-lock-comment-face)
           (3 ,font-lock-comment-delimiter-face))
         ;; TODO: this doesn't let you do newlines
         ;; Documentation comments.
         ("\\(--\\s-*|\\)\\(.*\\)\\(\n\\(--\\)\\(.*\\)\\)*"
           (1 ,font-lock-comment-delimiter-face)
           (2 ,font-lock-doc-face)
           (4 ,font-lock-comment-delimiter-face)
           (5 ,font-lock-doc-face))
         ;; Ordinary comments.
         ("\\(--\\)\s*\\(.*\\)"
           (1 ,font-lock-comment-delimiter-face)
           (2 ,font-lock-comment-face))
         ;; `%access`, `%default`, etc
         ("^%\\(\\w+\\)\\s-*\\(.+\\)"
           (1 ,idris-directive-face)
           (2 ,idris-directive-argument-face))
         ;; Definitions with keywords.
         (,(format "\\(%s\\) \\(\\w+\\)" (regexp-opt idris-definition-keywords))
           (1 ,idris-keyword-face)
           (2 ,idris-definition-face))
         ;; Type annotations.
         ;; TODO: this won't match, e.g. f:a
         ("^\\s-*\\(\\w+\\)\\s-+\\(:\\)\\s-+"
           (1 ,idris-definition-face)
           (2 ,idris-colon-face))
         ;; Operators
         (,idris-operator-regexp . ,idris-operator-face)
         ;; Vanilla definitions with = (and optionally let ... in ...)
         ;; TODO: clean up how parameters are picked up
         ("^\\s-*\\(\\w+\\)\s-*\\(.?*\\)\\(=\\)"
           (1 ,idris-definition-face)
           (2 ,idris-parameter-face)
           (3 ,idris-equals-face))
         ;; Identifiers
         ("\\w+" . ,idris-identifier-face)
         ;; TODO: operator definitions.
         ;; TODO: let ... in ...
))))

; Make the actual mode.
(define-derived-mode idris-mode fundamental-mode "Idris"
  (idris-load-faces))

(font-lock-add-keywords 'idris-mode
  '(("module" . idris-keyword-face)
     ("namespace" . idris-keyword-face)
     ("import" . idris-keyword-face)
     ("where" . idris-keyword-face)
     ("public" . idris-keyword-face)
     ("do" . idris-keyword-face)
     ("case" . idris-keyword-face)
     ("using" . idris-keyword-face)
     ("parameters" . idris-keyword-face)
     ("mutual" . idris-keyword-face)
     ("if" . idris-keyword-face)
     ("then" . idris-keyword-face)
     ("else" . idris-keyword-face)
     ("prefix" . idris-keyword-face)
     ("infix" . idris-keyword-face)
     ("infixr" . idris-keyword-face)
     ("infixl" . idris-keyword-face)
     ("pattern" . idris-keyword-face)
     ("term" . idris-keyword-face)
     ("syntax" . idris-keyword-face)
     ("of" . idris-keyword-face)
     ("intros" . idris-keyword-face)
     ("rewrite" . idris-keyword-face)
     ("exact" . idris-keyword-face)
     ("refine" . idris-keyword-face)
     ("trivial" . idris-keyword-face)
     ("focus" . idris-keyword-face)
     ("try" . idris-keyword-face)
     ("compute" . idris-keyword-face)
     ("solve" . idris-keyword-face)
     ("attack" . idris-keyword-face)
     ("with" . idris-keyword-face)
     ("dsl" . idris-keyword-face)
     ("instance" . idris-keyword-face)
     ("partial" . idris-keyword-face)
     ("total" . idris-keyword-face)))

; Automatically use idris-mode for .idr files.
(push '("\\.idr$" . idris-mode) auto-mode-alist)

(provide 'idris)

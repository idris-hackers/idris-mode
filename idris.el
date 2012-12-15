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

(defvar idris-keywords
  '("module" "namespace" "import" "where" "public" "do" "case"
     "using" "parameters" "mutual" "if" "then" "else" "prefix"
     "infix" "infixr" "infixl" "pattern" "term" "syntax" "of"
     "intros" "rewrite" "exact" "refine" "trivial" "focus" "try"
     "compute" "solve" "attack" "with" "dsl" "instance" "partial" "total")
  "Ordinary keywords.")

(defvar idris-definition-keywords
  '("data" "class" "codata" "record")
  "Keywords that introduce some identifier.")

(defun idris-load-faces ()
  (interactive)
  (setq font-lock-defaults
    `('(
         ;; Documentation comments.
         ("\\(--\\s-*|\\)\\(.*\\)\\(\n\\(--\\)\\(.*\\)\\)*"
           (1 ,font-lock-comment-delimiter-face)
           (2 ,font-lock-doc-face)
           (4 ,font-lock-comment-delimiter-face)
           (5 ,font-lock-doc-face))
         ;; Ordinary comments.
         ("\\(--\\)\\s-*\\(.*\\)"
           (1 ,font-lock-comment-delimiter-face)
           (2 ,font-lock-comment-face))
         ;; `%access`, `%default`, etc
         ("^%\\(\\w+\\) \\(.+\\)"
           (1 ,idris-directive-face)
           (2 ,idris-directive-argument-face))
         ;; Ordinary keywords.
         (,(regexp-opt idris-keywords) . ,idris-keyword-face)
         ;; Definitions with keywords.
         (,(format "\\(%s\\) \\(\\w+\\)" (regexp-opt idris-definition-keywords))
           (1 ,idris-keyword-face)
           (2 ,idris-definition-face))
         ;; Type annotations.
         ;; TODO: refine this -- it's matching on things like (x :: xs)
         ("\\(\\w+\\)\\s-*\\(:\\)"
           (1 ,idris-definition-face)
           (2 ,idris-colon-face))
         ;; Operators
         ("[-!#$%&\*\+./<=>\?@\\^|~:]+" . font-lock-variable-name-face)
         ;; Vanilla definitions with = (and optionally let ... in ...)
         ;; TODO: clean up how parameters are picked p
         ("\\(\\w+\\) \\(.?*\\)\\(=\\)"
           (1 ,idris-definition-face)
           (2 ,idris-parameter-face)
           (3 ,idris-equals-face))
         ;; TODO: {- comments -}
         ;; TODO: operator definitions.
         ;; TODO: let ... in ...
         ;; TODO: lists
))))

; Make the actual mode.
(define-derived-mode idris-mode fundamental-mode "Idris"
  (idris-load-faces))

; Automatically use idris-mode for .idr files.
(push '("\\.idr$" . idris-mode) auto-mode-alist)

(provide 'idris)

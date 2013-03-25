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
  '("attack" "case" "compute" "do" "dsl" "else" "exact" "focus" "if" "import"
    "in" "infix" "infixl" "infixr" "instance" "intros" "module" "mutual"
    "namespace" "of" "let" "parameters" "partial" "pattern" "prefix" "public"
    "refine" "rewrite" "solve" "syntax" "term" "then" "total" "trivial" "try"
    "using" "where" "with"))

(defconst idris-font-lock-defaults
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
         ;; Definitions using "with"
         ("^\\s-*\\(\\w+\\)\s-*\\(.?*\\)\\(with\\)\\(.?*\\)"
           (1 ,idris-definition-face)
           (2 ,idris-parameter-face)
           (3 ,idris-keyword-face)
           (4 ,idris-parameter-face))
         ;; Other keywords
         (,(regexp-opt idris-keywords 'words) . ,idris-keyword-face)
         ;; Identifiers
         ("\\w+" . ,idris-identifier-face)
         ;; TODO: operator definitions.
         ;; TODO: let ... in ...
)))



(provide 'idris-syntax)

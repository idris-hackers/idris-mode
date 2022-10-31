(require 'flycheck)
(require 'idris-mode)
(require 'flycheck-idris)
(require 'idris-commands)
(require 'inferior-idris)
(require 'idris-ipkg-mode)
(require 'cl-lib)
(require 'idris-test-utils)

;;; Code:

(flycheck-parse-error-with-patterns
(concat "Warning: We are about to implicitly bind the following lowercase names.\n"
        "You may be unintentionally shadowing the associated global definitions:\n"
        "  plus is shadowing Main.plus, Prelude.Types.plus\n"
        "\n"
        "Temp:5:3--5:4\n"
        " 1 | plus : Nat -> Nat -> Nat    \n"
        " 2 | plus x y = plus x \"w\"      \n"
        " 3 |                             \n"
        " 4 | data Foo : Nat -> Type where\n"
        " 5 |   F : Foo plus              \n"
        "       ^                         \n"
        "\n")
(flycheck-checker-get 'idris2 'error-patterns)
'idris2)

(flycheck-parse-error-with-patterns
 (concat "Error: While processing right hand side of plus. Ambiguous elaboration. Possible results:\n"
        "    Main.plus x (Builtin.fromString \"w\")          \n"
        "    Prelude.plus x (Builtin.fromString \"w\")       \n"
        "\n                                "
        "Temp:2:12--2:16                 \n"
        " 1 | plus : Nat -> Nat -> Nat   \n"
        " 2 | plus x y = plus x \"w\"      \n"
        "                ^^^^            \n"
        "\n")
 (flycheck-checker-get 'idris2 'error-patterns)
 'idris2)

(flycheck-parse-error-with-patterns
 (concat "Error: While processing right hand side of double. Undefined name rhs.\n"
         "\n"
         "Temp:8:12--8:15                   \n"
         " 4 | data Foo : Nat -> Type where \n"
         " 5 |   F : Foo plus               \n"
         " 6 |    a                          \n"
         " 7 | double : Nat -> Nat          \n"
         " 8 | double x = rhs x x           \n"
         "                ^^^               \n"
         "\n")
 (flycheck-checker-get 'idris2 'error-patterns)
 'idris2)


(null nil)

(provide 'idris-test-flycheck)
;;; idris-test-flycheck.el ends here

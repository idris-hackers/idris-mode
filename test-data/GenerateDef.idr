module GenerateDef

-- (idris-test-run-goto-char #'idris-generate-def)
data Test = A | B
--++
test : Test -> Int

{-
C-c C-a		idris-proof-search
C-c C-b		Prefix Command
C-c C-c		idris-case-dwim
C-c C-d		Prefix Command
C-c C-e		idris-make-lemma
C-c C-g		idris-generate-def
C-c C-l		idris-load-file
C-c RET		idris-add-missing
C-c C-n		idris-load-forward-line
C-c C-p		idris-load-backward-line
C-c C-r		idris-refine
C-c C-s		idris-add-clause
C-c C-t		idris-type-at-point
C-c C-w		idris-make-with-block
C-c C-z		idris-switch-to-repl
C-c C-S-a	idris-proof-search-next
C-c C-S-g	idris-generate-def-next
C-c C-SPC	prop-menu-by-completing-read
-}

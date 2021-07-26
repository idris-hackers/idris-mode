module MakeLemma

-- (idris-test-run-goto-char #'idris-make-lemma)
data Test = A | B

test : Test -> Test
--       +++++++++++
test x = ?make_lemma



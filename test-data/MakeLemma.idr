module MakeLemma

-- (idris-test-run-goto-char #'idris-make-lemma)
data Test = A | B

my_lemmaTest : Test -> Test
--       +++++++++++
my_lemmaTest x = ?my_lemmaTest_rhs

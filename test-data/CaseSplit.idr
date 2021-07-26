module CaseSplit
-- (idris-test-run-goto-char #'idris-case-split)
-- (idris-test-run-goto-char #'idris-case-dwim)
data Cases = Case1 | Case2 Int
testCase : Cases -> Cases
--       +++
testCase var = ?c

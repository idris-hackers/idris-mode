module AddMissing
-- (idris-test-run-goto-char #'idris-add-missing)
data Test = A | B

test : Test -> Int
--++
test A = 1

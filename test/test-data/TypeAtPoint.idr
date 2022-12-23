module TypeAtPoint

-- (idris-test-run-goto-char #'idris-type-at-point)

test : Int -> Integer

test2 : Int -> Integer
--+++
test2 = test

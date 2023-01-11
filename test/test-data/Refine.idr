module Refine

-- (idris-test-run-goto-char #'idris-refine "f")

data Test = A | B

f : Test -> Test
f = const A

test : Test -> Test

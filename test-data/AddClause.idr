module AddClause

data Test = A | B

--++++++++++++++++
test : Test -> Int

-- Regression test for:
-- idris-add-clause doesn't send a message when cursor is on a dash
-- https://github.com/idris-community/idris2-mode/issues/16
(-) : Nat

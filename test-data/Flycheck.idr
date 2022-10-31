plus : Nat -> Nat -> Nat
plus x y = plus x "w"

data Foo : Nat -> Type where
  F : Foo plus

double : Nat -> Nat
double x = rhs x x

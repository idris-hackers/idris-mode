module MetavarTest

plusComm : plus n m = plus m n
plusComm = ?plusComm_rhs

plusAssoc : plus x (plus y z) = plus (plus x y) z
plusAssoc = ?plusAssoc_rhs

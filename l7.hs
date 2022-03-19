data Extended = Infinity | Value Integer deriving Eq
data Formula a = Atom a |
                 Or (Formula a) (Formula a) |
                 And (Formula a) (Formula a) |
                 Not (Formula a) deriving Eq
data Set a = F (a->Bool) deriving Num
relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;

concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2)};
rev : l1 -> { l2 | Rmem(l2) = Rmem(l1) };

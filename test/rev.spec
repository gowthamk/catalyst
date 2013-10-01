relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;

rev : l1 -> { l2 | Rmem(l2) = Rmem(l1) };

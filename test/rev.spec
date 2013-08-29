relation Rhd (Cons(x,xs)) = {(x)};
relation Rmem = Rhd*;

rev : l1 -> { l2 | Rmem(l2) = Rmem(l1) };

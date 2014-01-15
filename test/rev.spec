relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};
relation Robs = Rob*;
relation Roas = Roa*;

concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2) /\
               Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};
rev : l1 -> { l2 | Rmem(l2) = Rmem(l1) /\ Robs(l2) = Roas(l1)};

(*
 
relation RId (x) = {(x)};
relation Rmem1 = Rmem[RId];
relation Robs1 = Robs[RId];
relation Roas1 = Roas[RId];

concat1 : l1 -> l2 -> { l | Rmem1(l) = Rmem1(l1) U Rmem1(l2) /\
               Robs1(l) = Robs1(l1) U Robs1(l2) U (Rmem1(l1) X Rmem1(l2))};
 *)

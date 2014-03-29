relation (Rhd R)(cons(x,xs)) = R(x) | nil = {()};
relation (Rmem R) = Rhd[R]*;
relation Rfst(Pair(x,y)) = {(x)};
relation Rsnd(Pair(x,y)) = {(y)};
primitive relation RId = \x. {(x)};

(R1,R2) map : l -> (x -> {v | R2(v) = R1(x)}) 
  -> {v | Rmem[R2](v) = Rmem[R1](l)};

mapPairs : l -> {v | Rmem[Rfst](v) = Rmem[RId](l)};

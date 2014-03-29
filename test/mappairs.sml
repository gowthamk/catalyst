exception Catalyst;

datatype ('a,'b) pair = Pair of 'a * 'b

fun map l f = case l of 
    [] => []
  | x::xs => (f x)::(map xs f)

fun mapPairs l = map <% (RId,Rfst) %>l (fn x => Pair (x,x))

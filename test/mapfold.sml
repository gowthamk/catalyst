exception Catalyst;

fun map l f = case l of
    [] => []
  | x::xs => (f x)::(map xs f)

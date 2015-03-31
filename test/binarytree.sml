exception Catalyst;

datatype 'a tree = Leaf of 'a
                 | Node of 'a tree * 'a * 'a tree

fun concat l1 l2 = case l1 of
    [] => l2
  | x::xs => x::(concat xs l2)

fun preOrder t = case t of
    Leaf x => [x]
  | Node z => 
    let
      val (l,x,r) = z
      val a1 = preOrder l
      val a2 = concat a1 [x]
      val a3 = concat a2 (preOrder r)
    in
      (*concat (concat (preOrder l) [x]) (preOrder r)*)
      a3
    end 

fun postOrder t = case t of
    Leaf x => [x]
  | Node (l,x,r) => concat (concat (postOrder l) (postOrder r)) [x]


  fun treefoldl t acc f = case t of
      L x => f acc x
    | B (lt,x,rt) => 
      let
        val a1 = foldtreel lt acc f
        val a2 = f a1 x
        val a3 = foldtreel rt a2 f
      in
        a3
      end

(R1,R2) foldtreel : {t1 : 'a tree} -> {b : 'b} -> 
    (f : acc -> x -> {v : 'b | R1(v) = R1(acc) U {(x)}} /\
                               R2(v) = R2(acc) U (R1(acc) X {(x)})) 
       -> {v : 'b | R1(v) = Rtmem(t1) U R1(b)
                 /\ R2(v) = Rtos(t1) U R2(b) U (R1(b) X Rtmem(t1))}

fun main _ = 
  let 
    val l = foldtreel
      (Node (Node (Leaf 1, 2, Leaf 3), 4, Node (Leaf 5, 6, Leaf 7)))
      [] (fn acc => fn x => concat acc [x])
    fun treeToString t = case t of
        (Leaf x) => Int.toString x
      | (Node (l,x,r)) => (treeToString l)^(Int.toString x)^(treeToString r)
    fun listToString l = case l of 
        []  => ""
      | (x::xs) => (Int.toString x) ^ (listToString xs)
  in
    TextIO.print (listToString l)
  end;

main ();

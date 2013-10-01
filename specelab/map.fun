functor ApplicativeMap (S : APPLICATIVE_MAP_STRUCTS) : APPLICATIVE_MAP = 
struct
  open S
  structure Key = Key
  structure Val = Value
  exception KeyNotFound of Key.t
  type t = (Key.t * Val.t) list
  val empty = []
  fun mem map k = List.exists (map, fn (k',v) => Key.equal (k,k'))
  fun find map k = 
    let
      val valop = List.peek (map, fn (k',v) => Key.equal (k,k'))
    in
      case valop of NONE => raise (KeyNotFound k)
        | SOME (k',v) => v
    end
  fun add map k v = (k,v)::map
  fun remove map k = List.remove (map, fn (k',_) => Key.equal(k,k'))
  fun toVector map = Vector.fromList map
  fun toString map = "{" ^ (Vector.toString (fn (k,v) => (Key.toString k) 
    ^ " => " ^(Val.toString v)) (Vector.fromList map)) ^ "}\n"
end

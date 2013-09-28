functor ApplicativeMap (S : APPLICATIVE_MAP_STRUCTS) : APPLICATIVE_MAP = 
struct
  open S
  structure Key = Key
  exception KeyNotFound
  type 'a t = (Key.t * 'a) list
  val empty = []
  fun mem map k = List.exists (map, fn (k',v) => Key.equal (k,k'))
  fun find map k = 
    let
      val valop = List.peek (map, fn (k',v) => Key.equal (k,k'))
    in
      case valop of NONE => raise KeyNotFound
        | SOME (k',v) => v
    end
  fun add map k v = (k,v)::map
  fun remove map k = List.remove (map, fn (k',_) => Key.equal(k,k'))
  fun toVector map = Vector.fromList map
end

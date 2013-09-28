signature KEY = 
sig
  type t
  val equal : t*t -> bool
end
signature APPLICATIVE_MAP_STRUCTS =
sig
  structure Key : KEY
end
signature APPLICATIVE_MAP = 
sig
  include APPLICATIVE_MAP_STRUCTS
  exception KeyNotFound
  type 'a t
  val empty : 'a t
  val mem : 'a t -> Key.t -> bool
  val find : 'a t -> Key.t -> 'a
  val add : 'a t -> Key.t -> 'a -> 'a t
  val remove : 'a t -> Key.t -> 'a t
  val toVector : 'a t -> (Key.t * 'a) vector
end

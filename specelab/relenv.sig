signature REL_ENV_STRUCTS = 
sig
  include SPEC_LANG
end
signature REL_ENV = 
sig
  include REL_ENV_STRUCTS
  type reldesc = { ty : unit,
                  map : (Con.t * Var.t vector option * RelLang.expr) 
                    vector}
  exception RelNotFound of RelLang.RelId.t
  type t
  val empty : t
  val mem : t -> RelLang.RelId.t -> bool
  val find : t -> RelLang.RelId.t -> reldesc
  val add : t -> (RelLang.RelId.t * reldesc) -> t
  val remove : t -> RelLang.RelId.t -> t
  val toVector : t -> (RelLang.RelId.t * reldesc) vector
  val toString : t -> string
end

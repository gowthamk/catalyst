signature VAR_ENV_STRUCTS = 
sig
  include SPEC_LANG
end
signature VAR_ENV = 
sig
  include VAR_ENV_STRUCTS
  type tyscheme
  sharing type tyscheme  = RefinementTypeScheme.t
  exception VarNotFound of Var.t
  type t
  val empty : t
  val mem : t -> Var.t -> bool
  val find : t -> Var.t -> tyscheme
  val add : t -> (Var.t * tyscheme) -> t
  val remove : t -> Var.t -> t
  val toVector : t -> (Var.t * tyscheme) vector
  val toString : t -> string
end

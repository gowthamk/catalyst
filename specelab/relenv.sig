signature REL_ENV_STRUCTS = 
sig
  structure SpecLang : SPEC_LANG
end
signature REL_ENV = 
sig
  include REL_ENV_STRUCTS
  
  type reldesc = { ty : SpecLang.ProjTypeScheme.t,
                  params : SpecLang.RelVar.t vector,
                  map : (SpecLang.Pat.t * SpecLang.RelLang.expr) 
                    vector}
  exception RelNotFound of SpecLang.RelId.t
  type t
  val empty : t
  val mem : t -> SpecLang.RelId.t -> bool
  val find : t -> SpecLang.RelId.t -> reldesc
  val add : t -> (SpecLang.RelId.t * reldesc) -> t
  val remove : t -> SpecLang.RelId.t -> t
  val toVector : t -> (SpecLang.RelId.t * reldesc) vector
  val layout : t -> Layout.t
end

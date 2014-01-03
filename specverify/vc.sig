signature VERIFICATION_CONDITION_STRUCTS =
sig
  include SPEC_LANG
  structure VE : VAR_ENV
  structure RE : REL_ENV
  sharing type RefinementTypeScheme.t = VE.tyscheme
  sharing type Var.t = VE.Var.t
  sharing type RelId.t = RE.SpecLang.RelId.t
  (*sharing type RelTypeScheme.t = RE.SpecLang.RelTypeScheme.t*)
end
signature VERIFICATION_CONDITION =
sig
  include VERIFICATION_CONDITION_STRUCTS

  datatype simple_pred = True
                       |  Base of Predicate.BasePredicate.t 
                       |  Rel of Predicate.RelPredicate.t

  datatype vc_pred =  Simple of simple_pred
                   |  Conj of simple_pred vector

  type tydbind = Var.t * TypeDesc.t

  type tydbinds = tydbind vector

  datatype t = T of tydbinds * vc_pred* simple_pred
  
  val fromTypeCheck : VE.t * RefinementType.t * RefinementType.t -> t vector

  val elaborate : RE.t * t -> t

  val layout : t vector -> Layout.t

  val layouts: t vector * (Layout.t -> unit) -> unit
end

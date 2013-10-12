signature VERIFICATION_CONDITION_STRUCTS =
sig
  include SPEC_LANG
  structure VE : VAR_ENV
  sharing type RefinementTypeScheme.t = VE.tyscheme
  sharing type Var.t = VE.Var.t
end
signature VERIFICATION_CONDITION =
sig
  include VERIFICATION_CONDITION_STRUCTS

  datatype t = T of TyDBinds.t * P.t * P.t

  val fromTypeCheck : VE.t * RefinementType.t * RefinementType.t -> t vector
end

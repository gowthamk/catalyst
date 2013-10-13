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

  datatype simple_pred = True
                       |  Base of Predicate.BasePredicate.t 
                       |  Rel of Predicate.RelPredicate.t
                       |  Conj of simple_pred * simple_pred 
  
  type tydbind = Var.t * TypeDesc.t

  type tydbinds = tydbind vector

  datatype t = T of tydbinds * simple_pred * simple_pred

  val fromTypeCheck : VE.t * RefinementType.t * RefinementType.t -> t vector
end

signature VERIFICATION_CONDITION_STRUCTS =
sig
  include SPEC_LANG
  structure VE : VAR_ENV
  sharing type RefinementTypeScheme.t = VE.tyscheme
end
signature VERIFICATION_CONDITION =
sig
  include VERIFICATION_CONDITION_STRUCTS

  datatype t = T of {pimpl : Predicate.BasePredicate.t * 
                             Predicate.BasePredicate.t,
                     rimpl : Predicate.RelPredicate.t * 
                             Predicate.RelPredicate.t}

  val fromTypeCheck : VE.t * RefinementType.t * RefinementType.t -> t vector
end

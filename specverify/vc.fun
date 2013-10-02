functor VerificationCondition (S : VERIFICATION_CONDITION_STRUCTS)
  : VERIFICATION_CONDITION =
struct
  open S
  structure P = Predicate.BasePredicate

  structure RP = Predicate.RelPredicate

  structure RefTy = RefinementType

  datatype t = T of {pimpl : P.t * P.t,
                     rimpl : RP.t * RP.t}

  fun fromTypeCheck (ve, subty, supty) =
    raise (Fail "unimp")
end

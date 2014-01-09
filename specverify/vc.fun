functor VerificationCondition (S : VERIFICATION_CONDITION_STRUCTS)
  : VERIFICATION_CONDITION =
struct
  open S
  structure P = Predicate
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure RefTy = RefinementType
  structure RefTyS = RefinementTypeScheme
  structure RefSS = RefinementSortScheme
  structure RelTy = RelType
  (*structure RelTyS = RelTypeScheme*)
  structure RI = RelId
  structure TyD = TypeDesc
  structure Env = TyDBinds
  structure L = Layout

  type sol = (RelVar.t * RelLang.ieatom) vector

  fun solveTypeCheck (ve, re, subSS, supSS) :
     sol vector =
    raise (Fail "Unimpl")
end

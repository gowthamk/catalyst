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

  type sol = (RelVar.t * RelLang.ieatom) vector
  val solveTypeCheck : VE.t * RE.t * RefinementSortScheme.t * 
    RefinementSortScheme.t -> sol vector

end

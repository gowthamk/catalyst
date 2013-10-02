signature VERIFICATION_CONDITION_STRUCTS =
sig
  include SPEC_LANG
  structure VE : VAR_ENV
  structure ANormalCoreML : A_NORMAL_CORE_ML
  (*
  sharing Con = ANormalCoreML.Con
  sharing TypeDesc = ANormalCoreML.TypeDesc
  sharing Tycon = ANormalCoreML.Tycon
  sharing Tyvar = ANormalCoreML.Tyvar
  sharing Var = ANormalCoreML.Var
  sharing Record = ANormalCoreML.Record
  sharing Const = ANormalCoreML.Const
  sharing Prim = ANormalCoreML.Prim
  sharing SourceInfo = ANormalCoreML.SourceInfo
  *)
  sharing type RefinementTypeScheme.t = VE.tyscheme
end
signature VERIFICATION_CONDITION =
sig
  include VERIFICATION_CONDITION_STRUCTS

  datatype t = T of {pimpl : Predicate.BasePredicate.t * 
                             Predicate.BasePredicate.t,
                     rimpl : Predicate.RelPredicate.t * 
                             Predicate.RelPredicate.t}

  val fromTypeCheck : VE.t * RefinementType.t * RefinementType.t -> t
end

signature REL_SPEC_STRUCTS = 
sig
  include ATOMS
  structure RefinementType : REFINEMENT_TYPE
  structure StructuralRelation : STRUCTURAL_RELATION
end
signature REL_SPEC =
sig
  include REL_SPEC_STRUCTS

  structure TypeSpec:
  sig
    datatype t = T of {var : Var.t,
                       spec : RefinementType.t}
  end

  datatype t = T of {reldecs : StructuralRelation.t Vector.t,
                     typespecs : TypeSpec.t Vector.t}
end

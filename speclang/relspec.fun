functor RelSpec (S : REL_SPEC_STRUCTS) : REL_SPEC = 
struct
  open S

  structure TypeSpec = 
  struct
    datatype t = T of {var : Var.t,
                       spec : RefinementType.t}
  end

  datatype t = T of {reldecs : StructuralRelation.t Vector.t,
                     typespecs : TypeSpec.t Vector.t}
end

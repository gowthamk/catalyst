functor RefinementType (S : REFINEMENT_TYPE_STRUCTS) : REFINEMENT_TYPE =
struct
  open S

  datatype t = T of {basety : TypeDesc.t,
                     bv : Var.t,
                     refinement : Predicate.t
                    }
end

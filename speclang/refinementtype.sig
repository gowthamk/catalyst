signature REFINEMENT_TYPE_STRUCTS= 
sig
  include ATOMS
  structure TypeDesc : TYPE_DESC
  structure Predicate : PREDICATE
end
signature REFINEMENT_TYPE = 
sig
  include REFINEMENT_TYPE_STRUCTS

  datatype t = T of {basety : TypeDesc.t,
                     bv : Var.t,
                     refinement : Predicate.t
                    }
end

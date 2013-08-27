functor StructuralRelation (S : STRUCTURAL_RELATION_STRUCTS) :
  STRUCTURAL_RELATION = 
struct
  open S

  datatype t = T of {id : RelId.t,
                     ty : unit,
                     map : (Con.t * Var.t list * term) Vector.t}
end

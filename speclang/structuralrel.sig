signature STRUCTURAL_RELATION_STRUCTS = 
sig
  include REL_LANG
end
signature STRUCTURAL_RELATION = 
sig
  include STRUCTURAL_RELATION_STRUCTS

  datatype t = T of {id : RelId.t,
                     ty : unit,
                     map : (Con.t * Var.t list * term) Vector.t}
end

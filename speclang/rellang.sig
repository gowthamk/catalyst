signature REL_LANG_STRUCTS=
sig
  include ATOMS
end

signature REL_LANG = 
sig
  include REL_LANG_STRUCTS

  structure RelId : ID

  datatype elem = Int of int
                | Bool of bool
                | Var of Var.t

  datatype expr = T of elem Vector.t
                | X of expr * expr
                | U of expr * expr
                | R of RelId.t * Var.t

  datatype term = Expr of expr
                | Star of RelId.t
end

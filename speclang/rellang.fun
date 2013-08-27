functor RelLang(S : REL_LANG_STRUCTS) : REL_LANG = 
struct
  open S

  structure RelId = Var

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

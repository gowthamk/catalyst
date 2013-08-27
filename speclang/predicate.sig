signature PREDICATE_STRUCTS =
sig
  include ATOMS
  structure RelLang : REL_LANG
end
signature PREDICATE = 
sig
  include PREDICATE_STRUCTS

  structure BasePredicate :
  sig
    datatype int_expr =   Plus of expr * expr
                        | Minus of expr * expr
                        | Mult of expr * expr
                        | Div of expr * expr

    and expr =  Int of int
              | Bool of bool
              | Var of Var.t
              | IntExpr of int_expr

    datatype t =  True 
                | Conj of t * t
                | Iff of t * t
                | Eq of expr * expr
  end

  structure RelPredicate :
  sig
    include REL_LANG

    datatype t = True
               | Eq of expr * expr
               | SubEq of expr * expr
               | Conj of t * t
  end

  datatype t =  T of BasePredicate.t * RelPredicate.t
end

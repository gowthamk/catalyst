functor Predicate(S : PREDICATE_STRUCTS):PREDICATE =
struct

  open S

  structure BasePredicate =
  struct
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

  structure RelPredicate = 
  struct
    open RelLang

    datatype t = True
               | Eq of expr * expr
               | SubEq of expr * expr
               | Conj of t * t
  end

  datatype t =  T of BasePredicate.t * RelPredicate.t
end

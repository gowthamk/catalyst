signature SPEC_LANG_STRUCTS =
sig
  include ATOMS
end
signature SPEC_LANG = 
sig
  include SPEC_LANG_STRUCTS

  structure TypeDesc : TYPE_DESC

  structure RelLang : 
  sig
    structure RelId : ID

    datatype elem = Int of int
                  | Bool of bool
                  | Var of Var.t
    datatype expr = T of elem list
                  | X of expr * expr
                  | U of expr * expr
                  | R of RelId.t * Var.t
    datatype term = Expr of expr
                  | Star of RelId.t
    val elemToString : elem -> string
    val exprToString : expr -> string
    val termToString : term -> string
  end

  structure StructuralRelation :
  sig
    
    datatype t = T of {id : RelLang.RelId.t,
                       ty : unit,
                       map : (Con.t * Var.t list option * RelLang.term) list}
    val toString : t -> string
  end

  structure Predicate : 
  sig
    structure BasePredicate :
    sig
      datatype int_expr =   Plus of expr * expr
                          | Minus of expr * expr
                          | Mult of expr * expr
      and expr =  Int of int
                | Bool of bool
                | Var of Var.t
                | IntExpr of int_expr
      datatype t =  True 
                  | Conj of t * t
                  | Iff of t * t
                  | Eq of expr * expr
      val toString : t -> string
    end

    structure RelPredicate :
    sig
      type expr = RelLang.expr
      datatype t = True
                 | Eq of expr * expr
                 | Sub of expr * expr
                 | SubEq of expr * expr
                 | Conj of t * t
      val toString : t -> string
    end
    datatype t =  T of BasePredicate.t * RelPredicate.t
    val toString : t -> string
    end

  structure RefinementType : 
  sig
    datatype t = Base of Var.t * TypeDesc.t * Predicate.t
               | Arrow of t*t
    val toString : t -> string
  end

  structure RelSpec : 
  sig
    structure TypeSpec:
    sig
      datatype t = T of Var.t * RefinementType.t
      val toString : t -> string
    end
    datatype t = T of StructuralRelation.t list * TypeSpec.t list
    val toString : t -> string
    val layout : t -> Layout.t
  end
end

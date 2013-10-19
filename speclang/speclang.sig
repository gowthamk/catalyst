signature SPEC_LANG_STRUCTS =
sig
  include ATOMS
end
signature SPEC_LANG = 
sig
  include SPEC_LANG_STRUCTS

  structure RelLang : 
  sig
    structure RelId : ID

    datatype elem = Int of int
                  | Bool of bool
                  | Var of Var.t
    datatype expr = T of elem vector
                  | X of expr * expr
                  | U of expr * expr
                  | R of RelId.t * Var.t
    datatype term = Expr of expr
                  | Star of RelId.t
    val elemToString : elem -> string
    val exprToString : expr -> string
    val termToString : term -> string
    val app : RelId.t * Var.t -> expr
    val union : expr * expr -> expr
    val crossprd : expr * expr -> expr
    val emptyexpr : unit -> expr
    val applySubsts : (Var.t * Var.t) vector -> expr -> expr
  end

  structure StructuralRelation :
  sig
    
    datatype t = T of {id : RelLang.RelId.t,
                       ty : unit,
                       map : (Con.t * Var.t vector option * RelLang.term)
                             vector}
    val conMapToString : (Con.t * Var.t vector option * RelLang.term) vector -> string
    val toString : t -> string
  end

  structure TyDBinds : APPLICATIVE_MAP where 
    type Key.t = Var.t and
    type Value.t = TypeDesc.t

  structure Predicate : 
  sig
    structure BasePredicate :
    sig
      datatype expr =  Int of int
                    | Bool of bool
                    | Var of Var.t
      datatype t =  Iff of t * t
                  | Eq of expr * expr
      val toString : t -> string
      val varEq : Var.t * Var.t -> t
      val applySubst : (Var.t * Var.t) -> t -> t
    end

    structure RelPredicate :
    sig
      type expr = RelLang.expr
      datatype t = Eq of expr * expr
                 | Sub of expr * expr
                 | SubEq of expr * expr
      val toString : t -> string
      val applySubst : (Var.t * Var.t) -> t -> t
    end
    datatype t =  True
               |  Base of BasePredicate.t 
               |  Rel of RelPredicate.t
               |  Exists of TyDBinds.t * t
               |  Conj of t * t
               |  Disj of t * t

    val layout : t -> Layout.t 
    val truee : unit -> t
    val conj : t*t -> t
    val conjR : t*RelPredicate.t -> t
    val conjP : t*BasePredicate.t -> t
    val applySubst : Var.t * Var.t -> t -> t
    val applySubsts : (Var.t * Var.t) vector -> t -> t
    val exists : TyDBinds.t * t -> t
    val disj : t*t -> t
    end

  structure RefinementType : 
  sig
    (*
     * Ideally, refinement should've been a Refinement.t.
     * But, circular dependency among structures is disallowed.
     * The following is not allowed as well:
     *  functor VarMap (type r) : APPLICATIVE_MAP where
     *    type Key.t = Var.t
     *    and Value.t = r
     *  datatype refinement = Disj of (VarMap(type r = t).t * ...)
     *)
    datatype t = Base of Var.t * TypeDesc.t * Predicate.t
               | Tuple of t vector
               | Arrow of t*t
               (* Records are tuples with fixed bound var *)
               (* Needs extension for {'a | r} list *)
    val layout : t -> Layout.t 
    val fromTyD : TypeDesc.t -> t
    val applySubsts : (Var.t * Var.t) vector -> t -> t
    val alphaRename : t -> Var.t -> t
    val mapBaseTy : t -> ((Var.t * TypeDesc.t * Predicate.t) -> 
          (Var.t * TypeDesc.t * Predicate.t)) -> t
    val mapTyD : t -> (TypeDesc.t -> TypeDesc.t) -> t
      
  end

  structure RefinementTypeScheme :
    sig
      datatype t = T of {tyvars : Tyvar.t vector,
                        refty : RefinementType.t }
      val generalize : Tyvar.t vector * RefinementType.t -> t
      val specialize: t -> RefinementType.t
      val instantiate : t * TypeDesc.t vector -> RefinementType.t
      val layout : t -> Layout.t 
    end

  structure RelSpec : 
  sig
    structure TypeSpec:
    sig
      datatype t = T of Var.t * RefinementType.t
      val layout : t -> Layout.t
    end
    datatype t = T of {reldecs : StructuralRelation.t vector,
                       typespecs : TypeSpec.t vector}
    val layout : t -> Layout.t
  end
end

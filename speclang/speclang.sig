signature SPEC_LANG_STRUCTS =
sig
  include ATOMS
end
signature SPEC_LANG = 
sig
  include SPEC_LANG_STRUCTS

  structure RelId : ID

  structure RelVar :
  sig
    include ID
    val eq : t*t -> bool
  end

  structure RelTyvar :
  sig
    type t
    val new : unit -> t
    val eq : t*t -> bool
    val toString : t -> string
  end

  structure RelType :
  sig
    datatype t = Tuple of TypeDesc.t vector
               | Reltyvar of RelTyvar.t
               | Cross of t * t
    val toString : t -> string
    val equal : (t*t) -> bool
    val newTuple : TypeDesc.t vector -> t
    val newVar : RelTyvar.t -> t
    val crossPrdType : (t*t) -> t

    val mapTyD : t -> (TypeDesc.t -> TypeDesc.t) -> t
    val foldTyD : t -> 'a -> (TypeDesc.t * 'a -> 'a) -> 'a
    val mapRelTyVar : t -> (RelTyvar.t -> t) -> t
    val foldRelTyVar : t -> 'a -> (RelTyvar.t * 'a -> 'a) -> 'a
    val instRelTyvars : (RelTyvar.t * t) vector * t -> t
    val instTyvars : (Tyvar.t * TypeDesc.t) vector * t -> t
  end

  structure RelTyConstraint :
  sig
    datatype t = Equal of RelType.t * RelType.t

    val solvePartial : t vector -> ((RelTyvar.t * RelType.t) vector 
      * (t vector))
    val new : RelType.t * RelType.t -> t
    val unifyRelTypes : RelType.t * RelType.t -> (t option * RelType.t)
    val mapTyD : t -> (TypeDesc.t -> TypeDesc.t) -> t
    val foldTyD : t -> 'a -> (TypeDesc.t * 'a -> 'a) -> 'a
    val mapRelTy : t -> (RelType.t -> RelType.t) -> t
    val foldRelTy : t -> 'a -> (RelType.t * 'a -> 'a) -> 'a
    val instTyvars : (Tyvar.t * TypeDesc.t) vector * t vector -> t
          vector
    val instRelTyvars : (RelTyvar.t * RelType.t) vector * t vector ->
          t vector
  end
  
  structure SimpleProjSort : 
  sig
    datatype t = Base of RelType.t
               | ColonArrow of TypeDesc.t * RelType.t
    val toString : t -> string
    val layout : t -> Layout.t
    val newBase : RelType.t -> t
    val newColonArrow : TypeDesc.t * RelType.t -> t
    val domain : t -> TypeDesc.t
    val range : t -> RelType.t

    val unify : t*t -> RelTyConstraint.t
    val mapTyD : t -> (TypeDesc.t -> TypeDesc.t) -> t
    val foldTyD : t -> 'a -> (TypeDesc.t * 'a -> 'a) -> 'a
    val mapRelTy : t -> (RelType.t -> RelType.t) -> t
    val foldRelTy : t -> 'a -> (RelType.t * 'a -> 'a) -> 'a
    val instTyvars : (Tyvar.t * TypeDesc.t) vector * t -> t
    val instRelTyvars : (RelTyvar.t * RelType.t) vector * t -> t
  end

  structure ProjSort :
  sig
    datatype t =  T of {paramsorts : SimpleProjSort.t vector,
                              sort : SimpleProjSort.t}
    val toString : t -> string
    val new : SimpleProjSort.t vector * SimpleProjSort.t -> t
    val domain : t -> TypeDesc.t
    val range : t -> RelType.t
    val paramSorts : t -> SimpleProjSort.t vector
    val foldTyD : t -> 'a -> (TypeDesc.t * 'a -> 'a) -> 'a
    val foldRelTy : t -> 'a -> (RelType.t * 'a -> 'a) -> 'a
    val instTyvars : (Tyvar.t * TypeDesc.t) vector * t -> t
    val instRelTyvars : (RelTyvar.t * RelType.t) vector * t -> t
  end

  structure ProjSortScheme : 
  sig
    datatype t = T of {reltyvars : RelTyvar.t vector,
                      constraints : RelTyConstraint.t vector,
                            sort : ProjSort.t}
    val toString : t -> string
    val generalize : RelTyConstraint.t vector * ProjSort.t -> t
    val specialize : t -> ProjSort.t
    val instantiate : (RelTyvar.t * RelType.t) vector * t ->
      (RelTyConstraint.t vector * ProjSort.t)
    val foldTyD : t -> 'a -> (TypeDesc.t * 'a -> 'a) -> 'a
    val instTyvars : (Tyvar.t * TypeDesc.t) vector * t -> t
    val domain : t ->TypeDesc.t
  end

  structure ProjTypeScheme :
  sig
    datatype t = T of {tyvars : Tyvar.t vector,
                       sortscheme : ProjSortScheme.t}
    val toString : t -> string
    val generalize : ProjSortScheme.t -> t
    val specialize : t -> ProjSortScheme.t
    val domain : t ->TypeDesc.t
    val tyvars : t -> Tyvar.t vector
    val instantiate : (Tyvar.t * TypeDesc.t) vector * t ->
        ProjSortScheme.t
  end

  structure RelLang : 
  sig
    datatype elem = Int of int
                  | Bool of bool
                  | Var of Var.t
    datatype instexpr = Relation of RelId.t
                      | Relvar of RelVar.t
                      | Inst of {args : ieatom vector,
                                  rel : RelId.t}
    and ieatom = Ie of instexpr
               | Re of expr
    (* expr could be a GADT with Relation ( List * ) kind *)
    and expr = T of elem vector
             | X of expr * expr
             | U of expr * expr
             | R1 of RelVar.t
             | R2 of instexpr * Var.t
    datatype term = Atom of ieatom
                  | Star of instexpr
    val elemToString : elem -> string
    val instExprToString : instexpr -> string
    val exprToString : expr -> string
    val termToString : term -> string
    val termOfExpr : expr -> term
    val instExprOfRel : RelId.t -> instexpr
    val instExprOfRelInst : RelId.t * ieatom vector -> instexpr
    val instExprOfRelVar : RelVar.t -> instexpr
    val ieatomOfInstExpr : instexpr -> ieatom
    val ieatomOfRel : RelId.t -> ieatom
    val ieatomOfRelVar : RelVar.t -> ieatom
    val ieatomOfExpr : expr -> ieatom
    (* pre-condition : expr vector not emtpy *)
    val mapRel : term -> (RelId.t -> instexpr) -> term
    val mapRVarToExpr : expr -> (RelVar.t -> expr) -> expr
    val mapRVarToIExpr : expr -> (RelVar.t -> instexpr) -> expr
    val instRelVars : ((RelVar.t * ieatom) vector * expr) -> expr
    val instRelVarsInTerm : ((RelVar.t * ieatom) vector * term) -> 
      term
    val app : instexpr * Var.t -> expr
    val union : expr * expr -> expr
    val crossprd : expr * expr -> expr
    val emptyexpr : unit -> expr
    val applySubsts : (Var.t * Var.t) vector -> expr -> expr
  end

  structure Pat :
  sig
    datatype value = Var of Var.t
                   | Tuple of Var.t vector
                   | Record of Var.t Record.t
    datatype t = Con of Con.t * value option
               | Value of value
    val toString : t -> string
  end

  structure StructuralRelation :
  sig
    datatype t = T of {id : RelId.t,
                       params : RelVar.t vector,
                       map : (Pat.t option * RelLang.term)
                             vector}
    val new : {id : RelId.t, params : RelVar.t vector,
               map : (Pat.t option * RelLang.term)
                     vector} -> t
    val patMapToString : (Pat.t option * RelLang.term) vector -> string
    val toString : t -> string
    val instantiate : ((RelVar.t * RelLang.ieatom) vector * t) -> 
      (Pat.t option * RelLang.term) vector
  end

  structure TyDBinds : APPLICATIVE_MAP where 
    type Key.t = Var.t and
    type Value.t = TypeDesc.t

  structure Predicate : 
  sig
    structure BasePredicate :
    sig
      datatype expr = Int of int
                    | Bool of bool
                    | Var of Var.t
      datatype t =  Eq of expr * expr
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
    val mapRExpr : t -> (RelLang.expr -> RelLang.expr) -> t
    end

  structure RefinementType : 
  sig
    datatype t = Base of Var.t * TypeDesc.t * Predicate.t
               | Tuple of (Var.t * t) vector
               | Arrow of (Var.t * t) * t

    val layout : t -> Layout.t 
    val fromTyD : TypeDesc.t -> t
    val toTyD : t -> TypeDesc.t
    val applySubsts : (Var.t * Var.t) vector -> t -> t
    val alphaRename : t -> t
    val alphaRenameToVar : t -> Var.t -> t
    val mapBaseTy : t -> ((Var.t * TypeDesc.t * Predicate.t) -> 
          (Var.t * TypeDesc.t * Predicate.t)) -> t
    val mapTyD : t -> (TypeDesc.t -> TypeDesc.t) -> t
    val instTyvars : (Tyvar.t * TypeDesc.t) vector * t -> t
    val mapRExpr : t -> (RelLang.expr -> RelLang.expr) -> t
      
  end

  structure RefinementSortScheme :
  sig
    type paramrefty 
    datatype t = T of {reltyvars : RelTyvar.t vector,
                       constraints : RelTyConstraint.t vector,
                       paramrefty : paramrefty }
    val paramRefTy : (RelVar.t * SimpleProjSort.t)
                      vector * RefinementType.t -> paramrefty
    val typedParams : paramrefty -> (RelVar.t * SimpleProjSort.t) vector
    val mapTyD : t -> (TypeDesc.t -> TypeDesc.t) -> t
    val instantiate : (RelTyvar.t * RelType.t) vector * t ->
          (RelTyConstraint.t vector * paramrefty)
    val instTyvars : (Tyvar.t * TypeDesc.t) vector * t -> t
    val instRelParams : (RelVar.t * RelLang.ieatom) vector * 
          paramrefty -> RefinementType.t
    val generalizeWith : RelTyvar.t vector * RelTyConstraint.t vector 
          * paramrefty -> t
    val generalize : RelTyConstraint.t vector * paramrefty -> t
    val fromRefTy : RefinementType.t -> t
    val toRefTy : t -> RefinementType.t
    val layout : t -> Layout.t
  end

  structure RefinementTypeScheme :
  sig
    datatype t = T of {tyvars : Tyvar.t vector,
                   sortscheme : RefinementSortScheme.t}
    val generalize : Tyvar.t vector * RefinementSortScheme.t -> t
    val specialize: t -> RefinementSortScheme.t
    val tyvars : t -> Tyvar.t vector
    val fromRefTy : RefinementType.t -> t
    val toRefTy : t -> RefinementType.t
    val instantiate : t * TypeDesc.t vector -> RefinementSortScheme.t
    val layout : t -> Layout.t 
  end

  structure RelSpec : 
  sig
    structure TypeSpec:
    sig
      datatype t = T of {name:Var.t,
                         params: RelVar.t vector,
                         refty : RefinementType.t}
      val layout : t -> Layout.t
    end
    datatype t = T of {reldecs : StructuralRelation.t vector,
                       typespecs : TypeSpec.t vector}
    val layout : t -> Layout.t
  end
end

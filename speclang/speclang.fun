functor SpecLang (S : SPEC_LANG_STRUCTS) : SPEC_LANG = 
struct
  open S

  structure RelLang =
  struct
    structure RelId = Var

    datatype elem = Int of int
                  | Bool of bool
                  | Var of Var.t
    datatype expr = T of elem vector
                  | X of expr * expr
                  | U of expr * expr
                  | R of RelId.t * Var.t
    datatype term = Expr of expr
                  | Star of RelId.t 
    val elemToString = fn el => case el of
        Int i => Int.toString i
      | Bool b => Bool.toString b
      | Var v => Var.toString v

    fun exprToString exp = case exp of
        T (elvec) => "{(" ^ (Vector.fold (elvec,"",fn(e,acc) => 
          (elemToString e) ^ acc)) ^ ")}"
      | X (e1,e2) => "(" ^ (exprToString e1) ^ " X " 
          ^ (exprToString e2) ^ ")"
      | U (e1,e2) => "(" ^ (exprToString e1) ^ " U " 
          ^ (exprToString e2) ^ ")"
      | R (rel,arg) => (RelId.toString rel) ^ "(" ^ (Var.toString arg) ^ ")"
    
    val exprToString = exprToString

    val termToString = fn trm => case trm of
        Expr e => exprToString e
      | Star r => (RelId.toString r) ^ "*"

    fun app (relId,var) = R(relId,var)
    fun union (e1,e2) = U (e1,e2)
    fun crossprd (e1,e2) = X (e1,e2)
    fun emptyexpr _ = T (Vector.fromList [])
    fun applySubsts substs rexpr = 
      let
        val doIt = applySubsts substs
        (* caution : telescoped substitutions *)
        fun subst v = Vector.fold (substs, v, fn ((new,old),v) =>
          if (Var.toString old = Var.toString v) then new else v)
        fun elemSubst elem = case elem of
            Var v => Var (subst v)
          | c => c
      in
      case rexpr of 
          T elemv => T (Vector.map (elemv,elemSubst))
        | X (e1,e2) => X (doIt e1, doIt e2)
        | U (e1,e2) => U (doIt e1, doIt e2)
        | R (relId,argvar) => R (relId, subst argvar)
      end
  end

  structure StructuralRelation =
  struct
    datatype t = T of {id : RelLang.RelId.t,
                       ty : unit,
                       map : (Con.t * Var.t vector option * RelLang.term) vector}

    fun conMapToString map =
      let
        val conmap = "{" ^ (Vector.toString (fn (c,vlo,trm) =>
            let
              val cstr = Con.toString c
              val vseq = case vlo of NONE => ""
                | SOME vl => Vector.toString Var.toString vl
              val trmstr = RelLang.termToString trm
            in
              cstr ^ vseq ^ " => " ^ trmstr
            end) map) ^ "}\n"
      in
        conmap
      end

    val toString = fn T{id,ty,map} =>
      let val relid = Var.toString id
          val conmap = conMapToString map
      in
        "relation " ^ relid ^ " = " ^ conmap
      end
  end

  structure Predicate =
  struct
    structure BasePredicate =
    struct
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
      val toString = fn _ => "T"
    end
    structure RelPredicate =
    struct
      type expr = RelLang.expr
      datatype t = True
                 | Eq of expr * expr
                 | Sub of expr * expr
                 | SubEq of expr * expr
                 | Conj of t * t
      fun toString rp = case rp of
          True => "T"
        | Eq (e1,e2) => (RelLang.exprToString e1) ^ " = "
            ^ (RelLang.exprToString e2)
        | Sub (e1,e2) => (RelLang.exprToString e1) ^ " C "
            ^ (RelLang.exprToString e2)
        | SubEq (e1,e2) => (RelLang.exprToString e1) ^ " C= "
            ^ (RelLang.exprToString e2)
        | Conj (e1,e2) => (toString e1) ^ " /\\ " ^ (toString e2)

      val toString = toString
    end
    datatype t =  T of BasePredicate.t * RelPredicate.t

    val toString = fn T(bp,rp) => "(" ^ (BasePredicate.toString bp) 
        ^ "," ^ (RelPredicate.toString rp)^ ")"

    fun truee _ = T (BasePredicate.True, RelPredicate.True)

    fun conj (T(p11,p12),T(p21,p22)) = T (BasePredicate.Conj(p11,p21),
      RelPredicate.Conj(p12,p22))

    fun conjR (T(p,r),r') = T (p,RelPredicate.Conj(r,r'))

    fun conjP (T(p,r),p') = T (BasePredicate.Conj(p,p'),r)
  end

  structure RefinementType =
  struct
    datatype t = Base of Var.t * TypeDesc.t * Predicate.t
               | Tuple of t vector
               | Arrow of t*t
                 (* Records are tuples with fixed bound var *)

    val symbase = "v_"

    val count = ref 0

    val genVar = fn _ => 
      let val id = symbase ^ (Int.toString (!count))
          val _ = count := !count + 1
      in
        Var.fromString id 
      end

    fun fromTyD tyD =
      let
        open TypeDesc
      in
        case tyD of
          Tarrow (td1,td2) => Arrow (fromTyD td1,fromTyD td2)
        | Trecord tdrec => Tuple (Vector.map (Record.toVector tdrec, 
            fn (lbl,td) => case fromTyD td of
              Base (_,td,pred) => Base (Var.fromString 
                (Field.toString lbl), td, pred)
            | refTy => refTy))
        | tyD => Base (genVar(), tyD, Predicate.truee())
      end

    fun toString rty = case rty of
          Base(var,td,pred) => "{" ^ (Var.toString var) ^ ":" 
            ^ (TypeDesc.toString td) ^ " | " ^ (Predicate.toString pred) ^ "}"
        | Tuple tv => Vector.toString toString tv
        | Arrow (t1,t2) => "(" ^ (toString t1) ^ ") -> (" 
          ^ (toString t2) ^ ")"

    val toString = toString
  end

  structure RefinementTypeScheme =
    struct
      datatype t = T of {tyvars : Tyvar.t vector,
                        refty : RefinementType.t}
    
      val generalize = fn (tyvars, refty) =>
        T {tyvars = tyvars, refty = refty}
      val specialize = fn (T {tyvars,refty}) =>
        refty
      fun toString (T {tyvars,refty}) =
        let
          val tyvstr = if Vector.isEmpty tyvars then ""
            else Vector.toString Tyvar.name tyvars
          val reftystr = RefinementType.toString refty
        in
          tyvstr ^ reftystr
        end
    end

  structure RelSpec =
  struct
    structure TypeSpec =
    struct
      datatype t = T of Var.t * RefinementType.t
      val toString = fn T(var,refty) =>
        (Var.toString var) ^ " : "  ^ 
          (RefinementType.toString refty)
    end
    datatype t = T of {reldecs : StructuralRelation.t vector,
                       typespecs : TypeSpec.t vector}
    val toString = fn T ({reldecs,typespecs,...}) =>
      let 
        val srs = Vector.toString StructuralRelation.toString reldecs          
        val tss = Vector.toString TypeSpec.toString typespecs
      in
        srs^tss
      end

    fun layout spec = Layout.str (toString spec)
    val layout = layout
  end 
end

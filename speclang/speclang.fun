functor SpecLang (S : SPEC_LANG_STRUCTS) : SPEC_LANG = 
struct
  open S
  structure L = Layout

  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  fun varStrEq (v1,v2) = (Var.toString v1 = Var.toString v2)
  fun varSubst (subst as (new,old)) v = if varStrEq (v,old) 
    then new else v

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

  structure TyDBinds =
  struct
    structure Key = 
    struct
      type t = Var.t
      val layout = L.str o Var.toString
      fun equal (v1,v2) = (Var.toString v1) = (Var.toString v2)
    end
    structure Map = ApplicativeMap (structure Key = Key
                                   structure Value = TypeDesc)
    open Map
  end

  structure Predicate =
  struct
    structure BasePredicate =
    struct
      datatype expr = Int of int
                    | Bool of bool
                    | Var of Var.t
      datatype t =  Iff of t * t
                  | Eq of expr * expr

      fun toString bp = case bp of
          Eq (Int i1,Int i2) => (Int.toString i1) ^ " = " 
            ^ (Int.toString i2)
        | Eq (Bool b1,Bool b2) => (Bool.toString b1) ^ " = " 
            ^ (Bool.toString b2)
        | Eq (Var v1, Var v2) => (Var.toString v1) ^ " = " 
            ^ (Var.toString v2)
        | Iff (t1,t2) => (toString t1) ^ " <=> " ^ (toString t2) 

      fun varEq (v1,v2) = Eq (Var v1,Var v2)

      fun applySubst subst t = 
      let
        val varSubst = varSubst subst
      in
        case t of
            Eq (Var v1, Var v2) => Eq (Var $ varSubst v1, Var $ varSubst v2)
          | Eq (Var v, e) => Eq (Var $ varSubst v, e)
          | Eq (e, Var v) => Eq (e, Var $ varSubst v)
          | Iff (t1,t2) => Iff (applySubst subst t1, applySubst subst t2)
      end
    end
    structure RelPredicate =
    struct
      type expr = RelLang.expr
      datatype t =   Eq of expr * expr
                 | Sub of expr * expr
                 | SubEq of expr * expr
                 
      fun toString rp = case rp of
          Eq (e1,e2) => (RelLang.exprToString e1) ^ " = "
            ^ (RelLang.exprToString e2)
        | Sub (e1,e2) => (RelLang.exprToString e1) ^ " C "
            ^ (RelLang.exprToString e2)
        | SubEq (e1,e2) => (RelLang.exprToString e1) ^ " C= "
            ^ (RelLang.exprToString e2)

      fun exprMap rp (f : RelLang.expr -> RelLang.expr) = case rp of
          Eq (e1,e2) => Eq (f e1, f e2)
        | Sub (e1,e2) => Sub (f e1, f e2)
        | SubEq (e1,e2) => SubEq (f e1, f e2)

      fun applySubst subst t = exprMap t 
        (RelLang.applySubsts $ Vector.new1 subst)
    end
    datatype t =  True
               |  Base of BasePredicate.t 
               |  Rel of RelPredicate.t
               |  Exists of TyDBinds.t * t
               |  Conj of t * t
               |  Disj of t * t

    fun layout t = case t of
        True => L.str "true" 
      | Base bp => L.str $ BasePredicate.toString bp
      | Rel rp => L.str $ RelPredicate.toString rp 
      | Exists (binds,t) => Pretty.nest ("exist",(TyDBinds.layout binds),
          layout t)
      | Conj (e1,e2) => L.align $ L.separateLeft ([(layout e1), (
          layout e2)],"/\\ ")
      | Disj (e1,e2) => L.align $ L.separateLeft ([(layout e1), (
          layout e2)],"\\/ ")

    fun truee _ = True

    fun conj (t1,t2) = Conj (t1,t2)

    fun conjR (t,r) = Conj (t,Rel r)

    fun conjP (t,p) = Conj (t,Base p)

    fun applySubst (subst as (new,old)) t = case t of
        True => True
      | Base bp => Base (BasePredicate.applySubst subst bp)
      | Rel rp => Rel (RelPredicate.applySubst subst rp)
      | Exists (tyDB,t) => if (TyDBinds.mem tyDB old)
            then Error.bug "Attempted substitution on existentially \
              \ quantified variable"
            else applySubst subst t
      | Conj (t1,t2) => Conj (applySubst subst t1, applySubst subst t2)
      | Disj (t1,t2) => Disj (applySubst subst t1, applySubst subst t2)

    (* telescoped substitutions *)
    fun applySubsts substs t = Vector.foldr (substs, t, fn (subst,t) =>
      applySubst subst t)

    fun exists (tyb,t) = Exists (tyb,t)

    fun disj (t1,t2) = Disj (t1,t2)
  end

  structure RefinementType =
  struct
    datatype t = Base of Var.t * TypeDesc.t * Predicate.t
          | Tuple of t vector
          | Arrow of t*t
          (* Records are tuples with fixed bound var *)
          (* Needs extension for {'a | r} list *)

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

    
    fun layout rty = case rty of
          Base(var,td,pred) => L.seq [L.str ("{" ^ (Var.toString var) 
            ^ ":" ^ (TypeDesc.toString td) ^ " | "), 
            Predicate.layout pred, L.str "}"]
        | Tuple tv => L.vector $ Vector.map (tv,layout)
        | Arrow (t1,t2) => L.align $ L.separateLeft ([layout t1, 
            layout t2]," -> ")

    fun mapBaseTy t f = case t of
        Base (v,t,p) => Base $ f (v,t,p)
      | Tuple tv => Tuple $ Vector.map (tv,fn t => mapBaseTy t f)
      | Arrow (t1,t2) => Arrow (mapBaseTy t1 f, mapBaseTy t2 f)

    fun mapTyD t f = mapBaseTy t (fn (v,t,p) => (v,f t,p)) 
      
    fun applySubsts substs refty = 
      mapBaseTy refty (fn (bv,t,pred) =>
        if Vector.exists (substs,fn(n,ol) => varStrEq (ol,bv))
          then Error.bug "Attempted substitution of bound var"
          else (bv,t,Predicate.applySubsts substs pred))

    fun alphaRenameToVar refty newbv = case refty of
        Base (bv,t,p) => Base (newbv,t,
          Predicate.applySubst (newbv,bv) p)
      | _ => Error.bug "alphaRename attempted on non-base type"

    fun alphaRename refty = alphaRenameToVar refty (genVar())

  end

  structure RefinementTypeScheme =
    struct
      datatype t = T of {tyvars : Tyvar.t vector,
                        refty : RefinementType.t}
    
      val generalize = fn (tyvars, refty) =>
        T {tyvars = tyvars, refty = refty}
      val specialize = fn (T {tyvars,refty}) =>
        refty
      fun layout (T {tyvars,refty}) =
        let
          val tyvlyt = L.vector $ Vector.map (tyvars,fn tyv =>
            L.str $ Tyvar.toString tyv)
          val reftylyt = RefinementType.layout refty
        in
          L.seq [tyvlyt,reftylyt]
        end
      fun instantiate (T{tyvars,refty},tydvec) =
        let
          val len = Vector.length
          val _ = assert (len tyvars = len tydvec,
            "insufficient number of type args")
          val substs = Vector.zip (tydvec,tyvars)
          (*
           * It is possible that we encounter a tyvar
           * that is not generalized in this RefTyS.
           * We do not panic.
           *)
        in
          RefinementType.mapTyD refty 
            (TypeDesc.instantiateTyvars substs)
        end
    end

  structure RelSpec =
  struct
    structure TypeSpec =
    struct
      datatype t = T of Var.t * RefinementType.t
      val layout = fn T(var,refty) => L.seq [
        L.str ((Var.toString var) ^ " : "),
        RefinementType.layout refty]
    end
    datatype t = T of {reldecs : StructuralRelation.t vector,
                       typespecs : TypeSpec.t vector}
    val layout = fn T ({reldecs,typespecs,...}) =>
      let 
        val srs = Vector.toString StructuralRelation.toString reldecs
        val tslyt = L.align $ Vector.toListMap (typespecs,
          TypeSpec.layout)
      in
        L.align [L.str srs,tslyt]
      end
  end 
end

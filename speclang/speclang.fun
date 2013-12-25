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

  structure RelId = Var

  structure RelVar = 
  struct
    open Var
    val equal = fn (rv1,rv2) => (toString rv1 = toString rv2)
  end

  structure RelTyvar =
  struct
    type t = Var.t

    val symbase = "'T"

    val count = ref 0

    val new = fn _ => 
      let val id = symbase ^ (Int.toString (!count))
          val _ = count := !count + 1
      in
        Var.fromString id 
      end

    fun equal (v1,v2) = (Var.toString v1 = Var.toString v2)
    val toString = Var.toString 
  end

  structure RelType =
  struct
    (*
     * Type of rexpr is always a tuple.
     * Type is empty tuple if and only if rexpr is {()}
     *)
    datatype t = Tuple of TypeDesc.t vector
               | Reltyvar of RelTyvar.t
               | Cross of t * t

    fun toString (Tuple tydv) = Vector.toString 
      TypeDesc.toString tydv
      | toString (Reltyvar t) = RelTyvar.toString t
      | toString (Cross (t1,t2)) = (toString t1) ^ "*" ^
          (toString t2)

    val toString = fn t => "{"^(toString t)^"}"

    (*
    fun unify (t1,t2) = case (t1,t2) of
        (Tuple _, Tuple _) => (assert (equal (t1,t2), 
          "Types not unifiable"); Vector.new0 ())
      | (Cross (),Tuple)
      | (Tuple, Cross)
      | 
     *)
    
    (* trivial equality *)
    fun equal (Tuple tydv1, Tuple tydv2) = 
      (Vector.length tydv1 = Vector.length tydv2) andalso
      Vector.forall2 (tydv1,tydv2,TypeDesc.sameType)
      | equal (Reltyvar v1, Reltyvar v2) = (RelTyvar.toString v1) =
          (RelTyvar.toString v2)
      | equal (Cross (t1,t2), Cross (t3,t4)) = (equal (t1,t3))
          andalso (equal(t2,t4))
      | equal _ = false

    fun newTuple tv = Tuple tv
    fun newVar rv = Reltyvar rv
      
    fun unionType (t1 as Tuple tydv1, t2 as Tuple tydv2) =
        (case (Vector.length tydv1, Vector.length tydv2) of
          (0,_) => t2 | (_,0) => t1
        | (n1,n2) => (assert (equal (t1,t2),"Union \
            \ incompatible types\n"); t1))
      | unionType _ = Error.bug ("Union \
            \ incompatible types\n")

    fun crossPrdType (t1 as Tuple tyds1, t2 as Tuple tyds2) =
        (case (Vector.length tyds1, Vector.length tyds2) of
          (0,_) => t1 | (_,0) => t2 
        | _ => Tuple $ Vector.concat [tyds1,tyds2])
      | crossPrdType (t1,t2) = Cross (t1,t2)

    fun mapTyD (Tuple tv) f = Tuple $ Vector.map (tv,f)
      | mapTyD (Cross (t1,t2)) f = Cross (mapTyD t1 f, mapTyD t2 f)
      | mapTyD t f = t
  end

  structure RelTyConstraint =
  struct
    datatype t = Equal of RelType.t * RelType.t

    type sol = (RelTyvar.t * RelType.t)

    (*
     * Solves constraints and returns solution.
     * Also returns residual constraints that it could not solve.
     * The problem, in general, is undecidable. For eg,
     * T1 X T2 = T3 X T4 cannot be solved.
     *)
    fun solvePartial (cs : t vector) : (sol vector * (t vector))
      =  raise (Fail "Unimpl")

    fun new (rt1,rt2) = Equal (rt1,rt2)
  end

  structure SimpleProjSort =
  struct
    datatype t = Base of RelType.t
               | ColonArrow of TypeDesc.t * RelType.t
    fun toString (ColonArrow (tyD,relKind)) =
      (TypeDesc.toString tyD)^" :-> "^(RelType.toString relKind)
      | toString (Base rt) = RelType.toString rt

    fun layout t = L.str $ toString t

    fun mapTyD (Base rt) f = Base (RelType.mapTyD rt f)
      | mapTyD (ColonArrow (tyd,rt)) f = ColonArrow (f tyd, 
          RelType.mapTyD rt f)
  
    fun newBase rt = Base rt

    fun newColonArrow (tyd,rt) = ColonArrow (tyd,rt)

    fun unify (Base rt1, Base rt2) = RelTyConstraint.new (rt1,rt2)
      | unify (ColonArrow (tyd1,rt1), ColonArrow (tyd2,rt2)) =
          (assert (TypeDesc.sameType (tyd1,tyd2), "Domains of projections\
            \ not unifiable");
           RelTyConstraint.new (rt1,rt2))
      | unify _ = raise (Fail "Projection expected to be 0th order\
          \ in one case, and 1st order in another")

    fun instTyvars (substs,t) =
      raise (Fail "Unimpl")

    fun instRelTyvars (substs,t) =
      raise (Fail "Unimpl")
  end

  structure ProjSort =
  struct
    datatype t =  T of {paramsorts : SimpleProjSort.t vector,
                        sort : SimpleProjSort.t}
    fun toString (T {paramsorts,sort}) = (Vector.toString
        SimpleProjSort.toString paramsorts) 
      ^ " -> " ^ (SimpleProjSort.toString sort)

    fun new (paramsorts, sort) = T {paramsorts = paramsorts,
      sort = sort}
  end

  structure ProjSortScheme =
  struct
    datatype t = T of {reltyvars : RelTyvar.t vector,
                      constraints: RelTyConstraint.t vector,
                            sort : ProjSort.t}

    fun toString (T{reltyvars,constraints,sort}) = 
      (Vector.toString RelTyvar.toString reltyvars)^" "^
        (ProjSort.toString sort)

    fun generalize (cstrs,sort) =
      raise (Fail "Unimpl")
  end

  structure ProjTypeScheme =
  struct
    datatype t = T of {tyvars : Tyvar.t vector,
                       sortscheme : ProjSortScheme.t}

    fun toString (T {tyvars, sortscheme}) = 
        (Vector.toString Tyvar.toString tyvars) 
      ^ ". " ^ (ProjSortScheme.toString sortscheme)

    fun generalize ss =
      raise (Fail "Unimpl")
  end

  structure RelLang =
  struct

    datatype elem = Int of int
                  | Bool of bool
                  | Var of Var.t
    datatype instexpr = Relation of RelId.t
                      | Relvar of RelVar.t
                      | Inst of {args: ieatom vector,
                                  rel : RelId.t}
    and ieatom = Ie of instexpr
               | Re of expr
    and expr = T of elem vector
             | X of expr * expr
             | U of expr * expr
             | R1 of RelVar.t
             | R2 of instexpr * Var.t

    datatype term = Expr of expr
                  | Star of instexpr

    val elemToString = fn el => case el of
        Int i => Int.toString i
      | Bool b => Bool.toString b
      | Var v => Var.toString v
    
    fun instExprToString (Relation r) = RelId.toString r
      | instExprToString (Relvar rv) = RelVar.toString rv
      | instExprToString (Inst {args, rel}) = "(" ^ 
        (RelId.toString rel) ^ " " ^ (L.toString $ L.seq $ L.separate 
          (Vector.toListMap (args, 
            fn ieatom => L.str $ ieatomToString ieatom), " ")) 
          ^ ")"

    and ieatomToString (Ie ie) = instExprToString ie
      | ieatomToString (Re re) = exprToString re

    and exprToString exp = case exp of
        T (elvec) => "{(" ^ (Vector.fold (elvec,"",fn(e,acc) => 
          (elemToString e) ^ acc)) ^ ")}"
      | X (e1,e2) => "(" ^ (exprToString e1) ^ " X " 
          ^ (exprToString e2) ^ ")"
      | U (e1,e2) => "(" ^ (exprToString e1) ^ " U " 
          ^ (exprToString e2) ^ ")"
      | R1 (relvar) => (RelVar.toString relvar)
      | R2 (instexp,arg) => (instExprToString instexp) ^ "(" 
          ^ (Var.toString arg) ^ ")"
    
    val exprToString = exprToString

    val termToString = fn trm => case trm of
        Expr e => exprToString e
      | Star instexp => (instExprToString instexp) ^ "*"
    fun termOfExpr expr = Expr expr
    fun instExprOfRel r = Relation r
    fun instExprOfRelVar rv = Relvar rv
    fun ieatomOfInstExpr ie = Ie ie
    fun ieatomOfRel r = ieatomOfInstExpr $ instExprOfRel r
    fun ieatomOfRelVar rv = ieatomOfInstExpr $ instExprOfRelVar rv
    fun ieatomOfExpr rexpr = Re rexpr
    fun instantiateRel (r,ieatoms) = Inst
      {args = ieatoms, rel = r}

    fun mapRelInstExpr (Relation rid) f = f rid
      | mapRelInstExpr (Inst {args,rel}) f = Inst {
          args = Vector.map (args, fn ieat => mapRelIEAtom ieat f),
          rel = rel}
    and mapRelIEAtom (Ie ie) f = Ie $ mapRelInstExpr ie f
      | mapRelIEAtom (Re expr) f = Re $ mapRelExpr expr f

    and mapRelExpr (X (e1,e2)) f = X (mapRelExpr e1 f, 
      mapRelExpr e2 f)
      | mapRelExpr (U (e1,e2)) f = U (mapRelExpr e1 f, 
      mapRelExpr e2 f)
      | mapRelExpr (R2 (ie,v)) f = R2 (mapRelInstExpr ie f, v)
      | mapRelExpr expr f = expr

    fun mapRel (Expr e) f = Expr $ mapRelExpr e f
      | mapRel (Star ie) f = Star $ mapRelInstExpr ie f
      
    fun app (relId,var) = R2 (relId,var)
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
        fun ieatomSubst (Ie ie) = Ie (ieSubst ie)
          | ieatomSubst (Re re) = Re (doIt re)
        and ieSubst (Inst {args,rel}) = Inst {args =
          Vector.map (args, ieatomSubst), rel = rel}
          | ieSubst ie = ie
      in
      case rexpr of 
          T elemv => T (Vector.map (elemv,elemSubst))
        | X (e1,e2) => X (doIt e1, doIt e2)
        | U (e1,e2) => U (doIt e1, doIt e2)
        | R1 rv => R1 rv
        | R2 (ie,argvar) => R2 (ieSubst ie, (subst argvar))
      end
  end

  structure Pat =
  struct
    datatype value = Var of Var.t
                   | Tuple of Var.t vector
                   | Record of Var.t Record.t
    datatype t = Con of Con.t * value option
               | Value of value
    fun valueToString (Var v) = Var.toString v
      | valueToString (Tuple vars) = Vector.toString Var.toString
          vars
      | valueToString (Record rc) = L.toString (Record.layout 
          { record = rc, 
            separator = ",", 
            extra = "", 
            layoutTuple = fn vars => L.str $ Vector.toString 
              Var.toString vars,
            layoutElt = fn v => L.str $ Var.toString v })

    fun toString (Con (c,patvalop)) = (Con.toString c) ^
        (case patvalop of NONE => "" 
          | SOME patval => " " ^ (valueToString patval))
      | toString (Value patval) = valueToString patval
  end

  structure StructuralRelation =
  struct
    datatype t = T of {id : RelId.t,
                       params : RelId.t vector,
                       map : (Pat.t option * RelLang.term)
                             vector}

    fun new data = T data
    fun patMapToString map =
      let
        val patmap = "{" ^ (Vector.toString (fn (pato,trm) =>
            let
              val patstr = case pato of NONE => ""
                | SOME pat => Pat.toString pat
              val trmstr = RelLang.termToString trm
            in
              patstr ^ " => " ^ trmstr
            end) map) ^ "}\n"
      in
        patmap
      end
    
    fun instantiate (T{id, params, map}) ieatoms =
      raise (Fail "unimpl")

    val toString = fn T{id,params,map} =>
      let
        val relstr = case Vector.length params of
          0 => RelId.toString id
        | _ => RelLang.instExprToString $
            RelLang.instantiateRel (id, Vector.map 
              (params,RelLang.ieatomOfRel))
        val patmap = patMapToString map
      in
        "relation " ^ relstr ^ " = " ^ patmap
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
      datatype t =  Eq of expr * expr

      fun toString bp = case bp of
          Eq (Int i1,Int i2) => (Int.toString i1) ^ " = " 
            ^ (Int.toString i2)
        | Eq (Bool b1,Bool b2) => (Bool.toString b1) ^ " = " 
            ^ (Bool.toString b2)
        | Eq (Var v1, Var v2) => (Var.toString v1) ^ " = " 
            ^ (Var.toString v2)

      fun varEq (v1,v2) = Eq (Var v1,Var v2)

      fun applySubst subst t = 
      let
        val varSubst = varSubst subst
      in
        case t of
            Eq (Var v1, Var v2) => Eq (Var $ varSubst v1, Var $ varSubst v2)
          | Eq (Var v, e) => Eq (Var $ varSubst v, e)
          | Eq (e, Var v) => Eq (e, Var $ varSubst v)
      end
    end
    structure RelPredicate =
    struct
      type expr = RelLang.expr
      datatype t = Eq of expr * expr
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
            else Exists (tyDB,applySubst subst t)
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
          | Tuple of (Var.t * t) vector
          | Arrow of (Var.t * t) * t
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
          Tarrow (td1,td2) => Arrow ((genVar (), fromTyD td1),
            fromTyD td2)
        | Trecord tdrec => Tuple (Vector.map (Record.toVector tdrec, 
            fn (lbl,td) => (Var.fromString $ Field.toString lbl, 
              fromTyD td)))
        | tyD => Base (genVar(), tyD, Predicate.truee())
      end

    
    fun layout rty = case rty of
          Base(var,td,pred) => L.seq [L.str ("{" ^ (Var.toString var) 
            ^ ":" ^ (TypeDesc.toString td) ^ " | "), 
            Predicate.layout pred, L.str "}"]
        | Tuple tv => L.vector $ Vector.map (tv, fn (v,t) => 
            L.seq [L.str $ Var.toString v, L.str ":", layout t])
        | Arrow ((v1,t1),t2) => L.align $ L.separateLeft (
            [L.seq [L.str $ Var.toString v1, L.str ":", layout t1], 
            layout t2]," -> ")

    fun mapBaseTy t f = case t of
        Base (v,t,p) => Base $ f (v,t,p)
      | Tuple tv => Tuple $ Vector.map (tv,fn (v,t) => 
          (v,mapBaseTy t f))
      | Arrow ((v1,t1),t2) => Arrow ((v1,mapBaseTy t1 f), 
          mapBaseTy t2 f)

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

  structure RefinementSortScheme =
  struct
    open RelLang
    type paramrefty = {params : (RelVar.t * 
                   (* abstract relations are simple projections *)
                              SimpleProjSort.t) vector,
                            refty : RefinementType.t }
    datatype t = T of {reltyvars :RelTyvar.t vector,
                       constraints : RelTyConstraint.t vector,
                        paramrefty : paramrefty }
    fun paramRefTy (params,refty) = {params = params, refty = refty}

    fun instRelTyvars (t,reltyv) = 
      raise (Fail "unimpl")

    fun instRelParams ({params,refty}, typedIeAtoms) =
      raise (Fail "unimpl")

    fun generalize (reltyvs, cs, prefty) =
      T {reltyvars = reltyvs, constraints = cs, paramrefty = prefty}

    fun fromRefTy refty = 
      let
        val empty = fn _ => Vector.new0 ()
      in
        generalize (empty(), empty(), paramRefTy (empty(), refty))
      end 

    fun toRefTy (T {paramrefty = {refty,...}, ...}) = refty

    fun instantiate (T{reltyvars, constraints, paramrefty}, reltys) = 
      raise (Fail "unimpl")

    fun mapTyD' {params, refty} f = {params = Vector.map 
          (params, fn (r,spt) =>
            (r,SimpleProjSort.mapTyD spt f)),
         refty = RefinementType.mapTyD refty f}

    fun mapTyD (T {reltyvars, constraints, paramrefty}) f =
      T {reltyvars = reltyvars, constraints = constraints, 
          paramrefty = mapTyD' paramrefty f}

    fun layout' {params,refty} = 
      let
        fun typedParamLyt (r,sprojty) = L.str $ 
          (RelVar.toString r) ^ " :: " ^
          (SimpleProjSort.toString sprojty)
        val paramslyt = L.vector $ Vector.map (params, typedParamLyt)
        val reftylyt = RefinementType.layout refty
      in
        L.seq [paramslyt, L.str ". ", reftylyt]
      end

    fun layout (T {reltyvars, constraints, paramrefty}) = 
      let
        val rtyvlyt = L.vector $ Vector.map (reltyvars,fn rtyv =>
          L.str $ RelTyvar.toString rtyv)
        val prflyt = layout' paramrefty
      in
        L.seq [rtyvlyt, L.str ". ", prflyt]
      end

  end

  structure RefinementTypeScheme =
  struct
    datatype t = T of {tyvars : Tyvar.t vector,
                 sortscheme : RefinementSortScheme.t}
  
    val generalize = fn (tyvars, sortscheme) =>
      T {tyvars = tyvars, sortscheme = sortscheme}

    fun fromRefTy refty = T {tyvars = Vector.new0 (),
      sortscheme = RefinementSortScheme.fromRefTy refty}

    val specialize = fn (T {tyvars,sortscheme}) =>
      sortscheme

    fun toRefTy (T{sortscheme, ...}) = 
      RefinementSortScheme.toRefTy sortscheme
    
    fun layout (T {tyvars,sortscheme}) =
      let
        val tyvlyt = L.vector $ Vector.map (tyvars,fn tyv =>
          L.str $ Tyvar.toString tyv)
        val sslyt = RefinementSortScheme.layout sortscheme
      in
        L.seq [tyvlyt, L.str ". ", sslyt]
      end

    fun instantiate (T{tyvars,sortscheme},tydvec) =
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
        RefinementSortScheme.mapTyD sortscheme
          (TypeDesc.instantiateTyvars substs)
      end
  end

  structure RelSpec =
  struct
    structure TypeSpec =
    struct
      datatype t = T of {name:Var.t,
                         params: RelVar.t vector,
                         refty : RefinementType.t}
      val layout = fn T {name=var,params,refty} => L.seq [
        L.str ((Var.toString var) ^ " : "),
        L.str (Vector.toString RelVar.toString params),
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

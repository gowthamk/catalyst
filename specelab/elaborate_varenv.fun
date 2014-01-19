functor ElaborateVarEnv (S : ELABORATE_VAR_ENV_STRUCTS) : ELABORATE_VAR_ENV = 
struct
  open S
  open SpecLang
  open ANormalCoreML
  structure Pat = SpecLang.Pat
  structure VE = VarEnv (structure Var = Var
                         structure SpecLang = SpecLang)
  structure RE = RelEnv (structure SpecLang = SpecLang)
  structure TyD = TypeDesc
  structure TyDB = TyDBinds
  structure RL = RelLang
  structure RelTy = RelType
  structure RefTy = RefinementType
  structure RefSS = RefinementSortScheme
  structure RefTyS = RefinementTypeScheme
  structure RP = Predicate.RelPredicate
  structure TypeSpec = RelSpec.TypeSpec
  structure SR = StructuralRelation
  structure SPS = SimpleProjSort
  structure PTS = ProjTypeScheme
  structure PSS = ProjSortScheme
  structure SPSBinds = ApplicativeMap (structure Key = 
                                       struct 
                                        open RelVar
                                        val equal = eq
                                       end
                                       structure Value = SPS)
  structure RelTyC = RelTyConstraint

  val assert = Control.assert
  val leneq = Control.leneq
  val tyVarEq = fn (v1,v2) => (Tyvar.toString v1 = Tyvar.toString v2)
  fun $ (f,arg) = f arg
  infixr 5 $

  structure Constraints =
  struct
    structure TyDConstraint =
    struct
      datatype t = Eq of Tyvar.t * TyD.t option
      type sol = Tyvar.t * TyD.t option
      fun new t = Eq t
    end

    structure TyDC = TyDConstraint

    datatype t = T of {tydcstrs : TyDC.t vector,
                   sortcstrs : RelTyC.t vector}
    type reltysol = (RelTyvar.t * RelType.t) 
    type solution = {tysolop : TyDC.sol vector,
                     sortsol : reltysol vector,
                     residuecs : RelTyC.t vector}

    val empty = T {tydcstrs = Vector.new0 (), 
                  sortcstrs = Vector.new0 ()}

    fun fromRCstrs rcs = T {tydcstrs = Vector.new0 (),
      sortcstrs = rcs}

    fun newRCstr (T {tydcstrs,sortcstrs}) rcstr =
      T{tydcstrs = tydcstrs, sortcstrs = Vector.concat 
        [sortcstrs, Vector.new1 rcstr]}

    fun newTCstr (T {tydcstrs,sortcstrs}) 
      (tcstr as (TyDC.Eq (tyvar,tydop))) : t =
      let
        val errMsg =  "Can't unify domain types of a relation"
        val known = Vector.exists (tydcstrs, 
          fn (TyDC.Eq (tyvar', tydop')) =>
          let
            val same = (Tyvar.toString tyvar = Tyvar.toString tyvar')
            val _ = case (same, tydop, tydop') of (false,_,_) => ()
              | (true,NONE,NONE) => ()
              | (true, SOME tyd, SOME tyd') => assert (TyD.sameType 
                  (tyd,tyd'),errMsg)
              | _ => raise (Fail "Can't unify domain types of a relation")
          in
            same
          end)
        val newtydcstrs = if known then tydcstrs else 
          Vector.concat [tydcstrs, Vector.new1 tcstr]
      in
        T {tydcstrs = newtydcstrs, sortcstrs = sortcstrs}
      end

    fun merge (T{tydcstrs=tydcstrs1,sortcstrs=sortcstrs1},
               T{tydcstrs=tydcstrs2,sortcstrs=sortcstrs2}) : t=
      let
        val newt = T {tydcstrs = tydcstrs1, sortcstrs = 
          Vector.concat [sortcstrs1, sortcstrs2]}
        val mergedt = Vector.fold (tydcstrs2, newt, 
          fn (tydcstr, newt) => newTCstr newt tydcstr)
      in
        mergedt
      end

    fun solve (T {tydcstrs, sortcstrs}) : solution =
      let
        val tysolop = Vector.map (tydcstrs, fn (TyDC.Eq e) => e)
        val (sols,residue) = RelTyC.solvePartial sortcstrs
      in
        {tysolop = tysolop, sortsol = sols, residuecs = residue}
      end
      
  end

  structure C = Constraints
  structure TyDC = C.TyDC
  structure RelTyC = RelTyConstraint

  fun mapToSRMap map = Vector.map (map, 
    fn (pat,rexpr) => (SOME pat,RelLang.termOfExpr rexpr))
  fun srMapToMap srmap = Vector.map (srmap, fn x => case x of 
      (SOME pat, RL.Atom (RL.Re e)) => (pat,e) 
    | _ => Error.bug "Impossible case of reldesc")

  fun elabDatBind (ve : VE.t) {cons,tyvars,tycon} =
    let
      val destTyD = TyD.makeTconstr (tycon, 
        Vector.toListMap (tyvars, TyD.makeTvar))
      val elabCon = fn ({arg,con},ve) =>
        let
          val vid = Var.fromString (Con.toString con)
          val conTyD = case arg of 
              NONE => destTyD
            | SOME argTy => TyD.makeTarrow (Type.toMyType argTy, 
                destTyD)
          val conSortS = RefSS.fromRefTy $ RefTy.fromTyD conTyD
          val conTyS = RefTyS.generalize (tyvars, conSortS)
        in
          VE.add ve (vid,conTyS)
        end
    in
      Vector.fold (cons, ve, elabCon)
    end

  fun unifyConArgs (ve : VE.t) (con : Con.t) (valpat : Pat.value) =
    let
      val conStr = Con.toString con
      val convid = Var.fromString conStr
      val lenstr = fn v => (Int.toString o Vector.length) v
      val conTy = RefTyS.toRefTy (VE.find ve convid)
        handle (VE.VarNotFound v) => Error.bug ("Could not find constructor "
          ^ conStr  ^ " in varenv\n")
      (* -- Function duplicated from SpecVerify -- *)
      val newLongVar = fn (var,fld) => Var.fromString $
        (Var.toString var)^"."^(Var.toString fld)
      open RefTy
    in
      case (conTy, valpat) of 
        (Base _, _) => Error.bug ("Nullary constructor "
          ^conStr^" applied to arguments")
      | (Arrow ((argv,Base (_,argTyD,_)),Base (_,datTyD,_)),
          Pat.Var v) => Vector.new1 
          (argv, v, argTyD, TyD.sameType (argTyD,datTyD))
        (*
         * We do not consider nested tuples in constructor
         * args as yet. Our syntax doesn't allow it.
         *)
      | (Arrow ((argv,Tuple tv), Base (_,datTyD,_)),
         Pat.Tuple vars) =>
          (assert (Vector.length tv = Vector.length vars,
          conStr ^ " expects "^ (lenstr tv) ^" args. " 
            ^ (lenstr vars) ^ " given");
         Vector.map2 (tv,vars,fn ((fldv, Base (_,argTyD,_)), var) =>
            (newLongVar (argv,fldv), var, argTyD, 
              TyD.sameType (argTyD,datTyD))))
      | (Arrow ((argv,Tuple tv), Base (_,datTyD,_)),
         Pat.Record rc) => raise (Fail "records unimpl")
      | _ => raise (Fail "Could not unify and determine rec args")
    end

  fun addRelToConTy (ve: VE.t) {id, pTyS, params, 
      conPatBind = (conPat,rexpr)} =
    let
      open Pat
      val (con,valpatop) = case conPat of
          Con (con,valpatop) => (con,valpatop)
        | _ => Error.bug "conPatBind contains non-conpat"
      val conStr = (Con.toString con)
      val convid = Var.fromString conStr
      val substs = case valpatop of NONE => Vector.new0 ()
        | SOME valpat => Vector.map (unifyConArgs ve con valpat, 
          fn (cvar,var,_,_) => (cvar,var))
      open RelLang
      val rexpr' = applySubsts substs rexpr
      val ie = instExprOfRelInst (id, Vector.map (params, 
        ieatomOfRelVar))
      val newref = fn var => RP.Eq (RelLang.app(ie,var),rexpr')
      val conRefTyS = VE.find ve convid
        handle (VE.VarNotFound v) => Error.bug ("Could not find \
          \constructor " ^ conStr ^ " in varenv\n")
      (* sanity check on tyvars *)
      val tyvars = RefTyS.tyvars conRefTyS
      val _ = Vector.foreach2 (ProjTypeScheme.tyvars pTyS, tyvars,
        fn (v1,v2) => assert (tyVarEq (v1,v2), "Assumption about \
          \ tyvar generalization failed"))
      (* annotate conRefTy with new refinement *)
      val conRefTy = case RefTyS.toRefTy conRefTyS of
          RefTy.Base (bv,tyd,pred) => RefTy.Base (bv,tyd, 
            Predicate.conjR (pred,newref bv))
        | RefTy.Arrow (arg,RefTy.Base (bv,tyd,pred)) => RefTy.Arrow(arg,
            RefTy.Base (bv,tyd, Predicate.conjR (pred,newref bv)))
        | _ => raise (Fail "Constructor type is neither base not arrow")
      (* 
       * Generalizing binders for relation sort and type of this
       * constructor are the same 
       *)
      val PSS.T {reltyvars,constraints,sort} = PTS.specialize pTyS
      val paramSorts = ProjSort.paramSorts sort
      val typedParams = Vector.zip (params,paramSorts)
      val paramRefTy = RefSS.paramRefTy (typedParams, conRefTy)
      val newRefSS = RefSS.generalizeWith (reltyvars, constraints,
        paramRefTy)
      val newRefTyS = RefTyS.generalize (tyvars,newRefSS)
    in
      VE.add (VE.remove ve convid) (convid,newRefTyS)
    end

  (*
   * Unify ranges of sort templates of params with ranges of argument 
   * ieatoms at application site, to determine instantiations of
   * reltyvars.
   *)
  fun unifyParamRanges (formals : RelType.t vector, 
      actuals : RelType.t vector) : (RelTyvar.t * RelType.t) vector =
    let
      (* 
       * Instantiations are substitutions for formal reltys 
       * that are reltyvars 
       *)
      open RelType
      val insts = Vector.keepAllMap2 (formals, actuals, 
        fn (relTy1,relTy2) => case relTy1 of 
          Reltyvar v => SOME (v,relTy2)
        | _ => NONE)
    in
      insts
    end

  (*
   * Unify domain of projection's sort template with type of its
   * argument at application site, to determine instantiations of
   * tyvars in domain.
   *)
  fun unifyProjDomain (domain : TyD.t, tyd : TyD.t) :
      (Tyvar.t * TypeDesc.t) vector = TyD.unify (domain,tyd)

  (*
   * Applied IEAtom datatype. 
   * We need this because types for colonarrow inst expressions
   * can be determined only when we know ML type to which it is
   * applied
   *)
  datatype ieatomapp = IEApp of RelLang.instexpr * TyD.t
                     | RE of RelLang.expr

  fun applyIEAtom (RelLang.Ie ie,tyd) = IEApp (ie,tyd)
    | applyIEAtom _ = raise (Fail "Relexpr cannot be applied")

  fun typeSynthIEApp (re, spsB, tyDB, ie, tyd:TyD.t) : (Constraints.t * 
      RelType.t) = 
    let
      open RelLang
      (*
       * Note: 1.In the type of inst-expr, left side of colonarrow
       * (domain) is always a known type.
       * 2. Only those tyvars in the domain are generalized in
       * the type of a projection
       *)
      fun relSortSchemeForApp (relId,tyd) =
        let
          val relName = RelId.toString relId
          val {ty = pTyS,...} = RE.find re relId handle 
                RE.RelNotFound _ => Error.bug ("Instantiating \
                  \unknown relation "^relName)
          val domain = ProjTypeScheme.domain pTyS
          val insts = unifyProjDomain (domain,tyd)
          val pSoS = ProjTypeScheme.instantiate (insts,pTyS)
        in
          pSoS
        end
      val emptycs = Constraints.empty
    in
      case ie of
        Relvar rv => 
        (* 
         * 'R : 'c -> {T1}, x : T2 => {'c := T2} |- 'R(x) : {T1} 
         *)
        let
          val rvty = SPSBinds.find spsB rv
          val (tyvar,relty) = case rvty of
              SPS.ColonArrow (TyD.Tvar tyvar, relty) => (tyvar,relty)
            | _ => raise (Fail "impossible case")
          val cs = C.newTCstr emptycs (TyDC.new (tyvar,SOME tyd))
        in
          (cs,relty)
        end
      | Relation relId => 
        let
          val pSoS = relSortSchemeForApp (relId,tyd)
          (* A non-parametric relation should have no generalized
           * reltyvars 
           *)
          val psort = ProjSortScheme.specialize pSoS
          val relTy = ProjSort.range psort
        in
          (emptycs, relTy)
        end
      | Inst {args,rel} =>
        let
          val pSoS = relSortSchemeForApp (rel,tyd)
          val pSo = ProjSortScheme.specialize pSoS
          val paramSorts = ProjSort.paramSorts pSo
          val _ = assert (leneq (paramSorts, args), "Projection\
            \ applied to too many/less args")
          (* paramsort domains are now concrete *)
          val (psDomainOps,psRanges) = Vector.unzip $ Vector.map 
            (paramSorts, fn ps => case ps of 
                SPS.Base relTy => (NONE,relTy)
              | SPS.ColonArrow (domain,relTy) => (SOME domain,relTy))
          val (asRanges,argcs) = Vector.map2AndFold (args, 
            psDomainOps, emptycs, fn (arg, psDomainOp, csacc) => 
              let
                val newIEAtom = case (arg,psDomainOp) of
                    (_,SOME ty) => applyIEAtom (arg,ty)
                  | (RelLang.Re re, NONE) => RE re
                  | _ => raise (Fail "Invalid paramSort-IEAtom combo")
                val (cs,asRange) = typeSynthIEAtomApp (re, spsB, 
                  tyDB, newIEAtom)
              in
                (asRange, C.merge (cs,csacc))
              end)
          val insts = unifyParamRanges (psRanges, asRanges)
          val (latentrcs,psort) = ProjSortScheme.instantiate 
            (insts, pSoS)
          val latentcs = C.fromRCstrs latentrcs
          val relTy = ProjSort.range psort
        in
          (C.merge (latentcs,argcs), relTy)
        end
    end

  (* Synthesizes type for ieatomapp, which is defined above *)
  and typeSynthIEAtomApp (re,spsB,tyDB,ieat) = case ieat of
      (IEApp (ie,tyd)) => typeSynthIEApp (re,spsB, tyDB, ie, tyd)
    | (RE rexpr) => typeSynthRExpr (re, spsB, tyDB, rexpr)
  (*
   * Synthesizes the type of rexpr in the given relational env.
   * Rel Env is constructed during elaboration, hence this function
   * is also part of elaboration.
   *)
  and typeSynthRExpr (re,spsB,tyDB,rexpr) : (Constraints.t *
      RelType.t) =
    let
      open RelLang
      open RelType
      val typeSynthRExpr = fn expr => 
        typeSynthRExpr (re,spsB,tyDB,expr)
      fun doIt (e1,e2) = 
        let
          val (cs1,ty1) =  typeSynthRExpr e1
          val (cs2,ty2) =  typeSynthRExpr e2
          val cs = Constraints.merge (cs1,cs2)
        in
          (cs,ty1,ty2)
        end
      fun typeSynthRElem elem = case elem of
          Int i => TyD.makeTconstr (Tycon.intInf,[])
        | Bool b => TyD.makeTconstr (Tycon.bool,[])
        | Var v => TyDBinds.find tyDB v 
            handle TyDBinds.KeyNotFound _ => Error.bug ("Var "
              ^(Var.toString v)^" not found in tydbinds")
      val emptycs = C.empty
    in
      case rexpr of 
        T elemvec => (emptycs, Tuple $ Vector.map (elemvec,
          typeSynthRElem))
      | X (e1,e2) => 
        let
          val (cs,ty1,ty2) = doIt (e1,e2)
        in
          (cs, crossPrdType (ty1, ty2))
        end 
      | U (e1,e2) => 
        let
          val (cs,ty1,ty2) = doIt (e1,e2)
          val (newrcOp, ty) = RelTyC.unifyRelTypes (ty1,ty2)
          val cs' = case newrcOp of NONE => cs
            | SOME newrc => C.newRCstr cs newrc
        in
          (cs', ty)
        end
      | R1 rv => 
        let
          val rvty = SPSBinds.find spsB rv 
            handle SPSBinds.KeyNotFound _ => raise (Fail "relvar not\
              \ found in params")
          val emptycs = Constraints.empty
          val (newcs,ty) = case rvty of 
              SPS.Base ty => (emptycs, ty)
            | SPS.ColonArrow (TyD.Tvar tyvar,ty) => (Constraints.newTCstr 
                emptycs (TyDC.new (tyvar,NONE)), ty)
            | _ => raise (Fail "Impossible case of rel param ty")
        in
          (newcs,ty)
        end
      | R2 (ie,arg) => 
        let
          val argTy = TyDBinds.find tyDB arg
            handle TyDBinds.KeyNotFound _ => Error.bug ("Type of "
              ^(Var.toString arg)^" not found in tydbinds")
          val (cs,ieAppty) = typeSynthIEApp (re, spsB, tyDB, ie, argTy)
        in
          (cs, ieAppty)
        end
    end

  fun typeSynthInstExpr ve re spsB ie : (C.t * SimpleProjSort.t) =
    let
      val baseR = case ie of RelLang.Relation r => r
        | RelLang.Inst {rel,...} => rel
        | _ => raise (Fail "Relparam cannot be a term")
      val {ty = pTyS,...} = RE.find re baseR 
        handle (RE.RelNotFound _) => raise (Fail $ "Ind of \
          \unknown relation: " ^ (RelId.toString baseR))
      val domainTyD = ProjTypeScheme.domain pTyS
      val (cs,relTy) = typeSynthIEApp (re, spsB, TyDB.empty, 
        ie, domainTyD)
      val sort = SPS.newColonArrow (domainTyD, relTy)
    in
      (cs,sort)
    end

  fun projTypeScheme ve re {params,map} : ProjTypeScheme.t = 
    let
      (* Initially, assign colonarrow types to all params *)
      val spsB = Vector.foldr (params, SPSBinds.empty, fn (r,spsB) => 
        SPSBinds.add spsB r (SPS.newColonArrow
          (TyD.makeTvar $ Tyvar.newNoname {equality=false},
           RelType.newVar $ RelTyvar.new ())))
      val assertNone = fn x => case x of NONE => ()
        | SOME _ => Error.bug "Impossible case of IETerm"
      (* Loop over map generating type constraints *)
      val (cs,sortOp) = Vector.fold (map, 
        (Constraints.empty, NONE), 
        fn ((patop,rterm),(csacc,(relTySOp : SPS.t option))) => 
          case (patop,rterm) of 
            (NONE, RelLang.Star ie) => (assertNone (relTySOp);
              (* types of inductive and simple versions match *)
              (fn (x,y) => (x, SOME y)) $ 
                typeSynthInstExpr ve re spsB ie)
          | (NONE, RelLang.Atom (RelLang.Ie ie)) => 
              (assertNone (relTySOp);
              (fn (x,y) => (x, SOME y)) $ 
                typeSynthInstExpr ve re spsB ie)
          | (SOME (Pat.Con (con,valpatop)), RL.Atom (RL.Re rexpr)) => 
            let
              val convid = Var.fromString (Con.toString con)
              val RefTyS.T {tyvars,sortscheme} = VE.find ve convid
              val refty = RefSS.toRefTy sortscheme
              val datTyD = case refty of RefTy.Base (_,datTyD,_) => datTyD
                | RefTy.Arrow (_,RefTy.Base (_,datTyD,_)) => datTyD
                | _ => raise (Fail "Impossible case")
              val tyDB = case valpatop of NONE => TyDBinds.empty
                | SOME valpat => Vector.fold (unifyConArgs ve con valpat, 
                  TyDBinds.empty, 
                  fn ((_,var,tyD,_),tyDB) => TyDBinds.add tyDB var tyD)
              val (cs,relTy) = typeSynthRExpr (re, spsB, tyDB, rexpr)
              val cs = Constraints.merge (cs,csacc)
              val (cs,range) = case relTySOp of NONE => (cs,relTy)
                | SOME (SPS.ColonArrow (d,relTy')) => (assert 
                  (TyD.sameType (datTyD,d), "Relation domains \
                    \ differ in case branches");
                  case RelTyC.unifyRelTypes (relTy,relTy') of
                    (NONE,newr) => (cs,newr)
                  | (SOME rc, newr) => (Constraints.newRCstr cs rc,
                      newr))
              val sort = SPS.newColonArrow (datTyD, range)
            in
              (cs,SOME sort)
            end
          | (SOME (Pat.Value v), RL.Atom (RL.Re rexpr)) =>
            let
              val _ = case relTySOp of NONE => ()
                | SOME _ => raise (Fail "A pattern of non-algeb\
                    \aic datatype should have only one case.")
              val tyDB = TyDB.empty
              val (tyDB,domainTy) = case v of
                  Pat.Var v => (fn t => (TyDB.add tyDB v t,t))
                    (TyD.makeTvar $ Tyvar.newNoname {equality=false})
                | Pat.Tuple vs =>
                  let
                    val tys = Vector.map (vs,fn _ =>
                      TyD.makeTvar $ Tyvar.newNoname {equality=false})
                    val tyDB = Vector.fold2 (vs,tys,tyDB, 
                      fn (v,t,tyDB) => TyDB.add tyDB v t)
                  in
                    (tyDB, TyD.makeTrecord $ Record.toVector $ 
                      Record.tuple tys)
                  end
                | Pat.Record vr => raise (Fail "Unimpl records")
              val (cs,rangeRTy) = typeSynthRExpr (re,spsB,tyDB,rexpr)
              val sort = SPS.newColonArrow (domainTy, rangeRTy)
            in
              (cs,SOME sort)
            end)
      (*  sortOp is the result of looping over the map.  *)
      val initSort = case sortOp of SOME sort => sort 
        | NONE => raise (Fail "impossible case of sort")
      (* Solve constraints. tyvar constraints are completely solved.
       * reltyvar constraints can contain unsolvable residue.
       *)
      val {tysolop,sortsol,residuecs} = Constraints.solve cs
      (* Correction for params which were wrongly assigned colonarrow
       * sorts
       *)
      val {yes=nullable,no} = Vector.partition (tysolop, 
        fn (tyvar,tydop) => case tydop of NONE => true | _ => false)
      val tysol = Vector.map (no, fn (tyvar,SOME tyd) => 
        (tyvar,tyd))
      val initCAPS = Vector.map (params, SPSBinds.find spsB)
      val initPS = Vector.map (initCAPS, 
        fn paramSort => case paramSort of 
          SPS.ColonArrow (TyD.Tvar (tyvar),rt) =>
            if (Vector.exists (nullable, fn (v,_) => 
              (Tyvar.toString v = Tyvar.toString tyvar)))
            then SPS.newBase rt
            else paramSort
          | _ => Error.bug "impossible case paramSort")
      (* Apply solution *)
      val paramSorts = Vector.map (initPS, fn (srt) => 
        SPS.instTyvars (tysol, SPS.instRelTyvars (sortsol,srt)))
      val sort = SPS.instTyvars (tysol, SPS.instRelTyvars 
        (sortsol, initSort))
      val projSort = ProjSort.new (paramSorts, sort)
      val sortScheme = ProjSortScheme.generalize
        (residuecs,projSort)
      (* We don't have tyvars to supply to generalize *)
      val typeScheme = ProjTypeScheme.generalize
        sortScheme
    in
      typeScheme
    end

  fun typeCheckRelPred re spsB tyDB rp : C.t =
    let
      open Predicate.RelPredicate
      val f = fn e =>  typeSynthRExpr (re,spsB,tyDB,e)
      val g = fn (e1,e2) =>
        let
          val (cs1,rt1) = f e1
          val (cs2,rt2) = f e2
        in
          C.newRCstr (C.merge (cs1,cs2)) 
            (RelTyC.new (rt1,rt2))
        end
    in
      case rp of
        Eq x => g x | SubEq x => g x  | Sub x => g x
    end

  fun typeCheckPred re spsB tyDB p : C.t =
    let
      open Predicate
      val f = typeCheckPred re spsB
      val mergeTyDBs = fn (t1,t2) => Vector.fold (TyDB.toVector t2,t1,
        fn ((v,ty),t1) => TyDB.add t1 v ty)
      val g = fn (p1,p2) => C.merge (f tyDB p1, f tyDB p2)
    in
      case p of
        Rel rp => typeCheckRelPred re spsB tyDB rp
      | Exists (exTyDB, p') => f (mergeTyDBs (tyDB,exTyDB)) p'
      | Conj x => g x
      | _ => C.empty
    end

  fun relVarsInRefTy refTy = 
    let
      val {add, ...} = List.set {equals = RelVar.eq, 
        layout = RelVar.layout }
    in
      Vector.fromList $ RefTy.foldRExpr refTy [] 
        (fn (e,acc) => RelLang.foldRVar e acc
          (fn (rv,acc2) => add (acc2,rv)))
    end 
  (*
   * TODO: Refractor the common parts of this function and
   * ProjTypeScheme into one function.
   *)
  fun sortSchemeOfRefTy re refTy =
    let
      val params = relVarsInRefTy refTy
      (* Initially, assign colonarrow types to all params *)
      val spsB = Vector.foldr (params, SPSBinds.empty, fn (r,spsB) => 
        SPSBinds.add spsB r (SPS.newColonArrow
          (TyD.makeTvar $ Tyvar.newNoname {equality=false},
           RelType.newVar $ RelTyvar.new ())))
      val f = typeCheckPred re spsB
      fun doItRefTy tyDB refTy = case refTy of
        RefTy.Base (v,tyd,p) => f (TyDB.add tyDB v tyd) p
      | RefTy.Tuple vts => raise (Fail "Unimpl uncurry")
      | RefTy.Arrow ((v,argTy),resTy) =>
        let
          val cs1 = doItRefTy tyDB argTy 
          val tyDB' = TyDBinds.add tyDB v (RefTy.toTyD argTy)
          val cs2 = doItRefTy tyDB' resTy
        in
          C.merge (cs1,cs2)
        end
      val cs = doItRefTy TyDBinds.empty refTy
      val {tysolop,sortsol,residuecs} = Constraints.solve cs
      (* Correction for params which were wrongly assigned colonarrow
       * sorts
       *)
      val {yes=nullable,no} = Vector.partition (tysolop, 
        fn (tyvar,tydop) => case tydop of NONE => true | _ => false)
      val tysol = Vector.map (no, fn (tyvar,SOME tyd) => 
        (tyvar,tyd))
      val initCAPS = Vector.map (params, SPSBinds.find spsB)
      val initPS = Vector.map (initCAPS, 
        fn paramSort => case paramSort of 
          SPS.ColonArrow (TyD.Tvar (tyvar),rt) =>
            if (Vector.exists (nullable, fn (v,_) => 
              (Tyvar.toString v = Tyvar.toString tyvar)))
            then SPS.newBase rt
            else paramSort
          | _ => Error.bug "impossible case paramSort")
      (* Apply solution *)
      val paramSorts = Vector.map (initPS, fn (srt) => 
        SPS.instTyvars (tysol, SPS.instRelTyvars (sortsol,srt)))
      val typedParams = Vector.zip (params, paramSorts)
      val paramRefTy = RefSS.paramRefTy (typedParams, refTy)
      val sortScheme = RefSS.generalize (residuecs, paramRefTy)
    in
      sortScheme
    end

  fun elabSRBind (re: RE.t)(ve : VE.t) {id,params,map} =
    let
      (* Replace RIds with RelVars wherever applicable *)
      fun paramOf rid = Vector.peek (params, fn rvar => 
        RelVar.toString rvar = RelId.toString rid)
      val map1 = Vector.map (map, fn (patop, rterm) =>
        let
          val f = fn rid => case paramOf rid of 
              NONE => RelLang.instExprOfRel rid
            | SOME rvar => RelLang.instExprOfRelVar rvar
          val rterm' = RelLang.mapRel rterm f
        in
          (patop, rterm')
        end)
      (*
       * An instexpr is a fully instantiated parametric relation.
       * Expand instexpr and return a reldesc map.
       *)
      fun expandInstExpr ie = 
        let
          val (relId,args) = case ie of 
              RelLang.Relation rid => (rid, Vector.new0 ())
            | RelLang.Relvar _ => raise (Fail "Relvar can't be\
              \ a top-level instexpr")
            | RelLang.Inst {args,rel} => (rel,args)
          val {ty, params, map} = RE.find re relId 
            handle (RE.RelNotFound r) => raise (Fail 
              ("Ind of unknown relation : "^(RelId.toString r)))
          val _ = assert (leneq (args,params), "Insufficient/too \
            \ many args to relation : "^(RelId.toString relId))
          val eqs = Vector.zip (params,args)
          val sr = SR.new {id = relId, 
              params = params, map = mapToSRMap map}
          val map' = srMapToMap $ SR.instantiate (eqs,sr)
        in
          map'
        end
      val map2 = (Vector.concatV o Vector.map) (map1, 
        fn (patop,rterm) => case (patop,rterm) of 
          (SOME pat,RelLang.Atom (RL.Re e)) => Vector.new1 (pat,e)
        | (NONE, RL.Atom (RL.Ie ie)) => expandInstExpr ie
        | (NONE, RL.Star ie) => Vector.map (expandInstExpr ie, 
            fn (pat,rexpr) => case pat of 
              (Pat.Value _) => (pat,rexpr)
            | (Pat.Con (con, NONE))=> (pat,rexpr)
            | (Pat.Con (con, SOME valpat)) => 
              let
                val recvars = Vector.map ((#yes o Vector.partition) 
                  (unifyConArgs ve con valpat, fn (_,_,_,isrec) => isrec),
                    fn (cvar,_,_,_) => cvar)
                val recRApps = Vector.map (recvars, fn var => 
                  RelLang.app (ie,var))
                val recRAppsUnion = Vector.fold (recRApps,
                  RelLang.emptyexpr(), RelLang.union)
                val rexpr' = RelLang.union (rexpr, recRAppsUnion)
              in
                (pat,rexpr')
              end)
        | _ => Error.bug "Impossible patop-rterm combination\n")
      (* Annotate reldesc with type (ProjSort) information *)
      val typeScheme = projTypeScheme ve re {params = params, 
          map = map1}
      (* sanity check *)
      val paramSorts = ProjSort.paramSorts $ PSS.specialize 
        $ PTS.specialize typeScheme
      val _ = assert (leneq (params,paramSorts), "len params != \
        \len paramSorts for "^(RelId.toString id))
    in
      RE.add re (id,{ty=typeScheme, params = params, map=map2})
    end

  fun elabTypeSpec (ts as (TypeSpec.T {params,refty, ...})) 
    : RefTyS.t =
    let
      (* Replace RIds with RelVars wherever applicable *)
      fun paramOf rid = Vector.peek (params, fn rvar => 
        RelVar.toString rvar = RelId.toString rid)
      val refty' = RefTy.mapRel refty (fn rid =>
        case paramOf rid of 
          NONE => RelLang.instExprOfRel rid
        | SOME rvar => RelLang.instExprOfRelVar rvar)
      val (tyvars, reltyvars, typedParams) = Vector.unzip3 $
       Vector.map (params, fn r =>
        let
          val tyvar = Tyvar.newNoname {equality=false}
          val reltyvar = RelTyvar.new ()
        in
          (tyvar, reltyvar, (r, SPS.newColonArrow (TyD.makeTvar tyvar,
            RelType.newVar reltyvar)))
        end)
      val pRefTy = RefSS.paramRefTy (typedParams, refty')
      val refSS = RefSS.generalizeWith (reltyvars, Vector.new0 (), 
        pRefTy)
      val refTyS = RefTyS.generalize (tyvars,refSS)
    in
      refTyS
    end

  fun elaborate (Program.T {decs = decs}) (RelSpec.T {reldecs, typespecs}) =
    let
      val initialVE = Vector.fold (decs,VE.empty,fn (dec,ve) =>
        case dec of Dec.Datatype datbinds => Vector.fold (datbinds, ve,
          fn (datbind,ve)   => elabDatBind ve datbind) 
          | _ => ve)
      val elabRE = Vector.fold (reldecs, RE.empty, 
        fn(StructuralRelation.T srbind,re) => 
          elabSRBind re initialVE srbind)
      val refinedVE = Vector.fold (RE.toVector elabRE, initialVE, 
        fn ((id,{ty,params,map}),ve) => 
          let
            val domainTyD = PTS.domain ty
            val isADT = case domainTyD of TyD.Tconstr _ => true
              | _ => false
          in
            case isADT of false => ve
            | true => Vector.fold (map, ve, fn (conPatBind, ve) => 
                addRelToConTy ve {id = id, pTyS = ty, params = params, 
                  conPatBind = conPatBind})
          end)
      val fullVE = Vector.fold (typespecs, refinedVE, 
        fn (ts as TypeSpec.T {name=f, ...},ve) => VE.add ve (f,
          elabTypeSpec ts))
    in
      (fullVE,elabRE)
    end
end

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
      (SOME pat,RelLang.Expr e) => (pat,e) 
    | _ => Error.bug "reldesc has star\n")

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
      val ie = instantiateRel (id, Vector.map (params, 
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
      val newRefSS = RefSS.generalize (reltyvars, constraints,
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
      (Tyvar.t * TypeDesc.t) vector =
    let
      open TyD
      val errMsg = fn _ => "The two domain types cannot be unified: "
        ^(TyD.toString domain)^", "^(TyD.toString tyd)
      val fldStrEq = fn (fld1,fld2) => (Field.toString fld1 = 
        Field.toString fld2)
    in
      case (domain,tyd) of
        (Tvar v1, _) => Vector.new1 (v1,tyd)
      | (Tconstr (tycon1,tyds1), Tconstr (tycon2,tyds2)) =>
        let
          val _ = assert (Tycon.toString tycon1 = Tycon.toString
            tycon2, errMsg ())
          val insts = Vector.concatV $ Vector.map2 (
            Vector.fromList tyds1, Vector.fromList tyds2,
            unifyProjDomain)
        in
          insts
        end
      | (Trecord tr1, Trecord tr2) =>
        let
          val vec1 = Record.toVector tr1
          val vec2 = Record.toVector tr2
          val insts = Vector.concatV $ Vector.map (vec1,
            fn (fld1, tyd1) => case Vector.peek (vec2, 
                fn (fld2, _) => fldStrEq (fld1,fld2)) of
              SOME (_,tyd2) => unifyProjDomain (tyd1,tyd2)
            | NONE => raise (Fail $ errMsg()))
        in
          insts
        end
      | _ => raise (Fail $ errMsg ())
    end

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
      val typeSynthRExpr = fn expr => typeSynthRExpr (re,spsB,tyDB,expr)
      fun unifyRelTypes (rt1,rt2) =
        let
          val n1 = Vector.length $ relTyVarsIn rt1
          val n2 = Vector.length $ relTyVarsIn rt2
          val rc = RelTyC.new (rt1,rt2)
          val rt = if n2 > n1 then rt2 else rt1
        in
          (rc,rt)
        end
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
          val (newrc, ty) = unifyRelTypes (ty1,ty2)
        in
          (C.newRCstr cs newrc, ty)
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
          val (cs,ieAppty) = typeSynthIEApp (re, spsB, tyDB, ie, argTy)
        in
          (cs, ieAppty)
        end
    end

  fun projTypeScheme ve re {params,map} : ProjTypeScheme.t = 
    let
      (* Initially, assign colonarrow types to all params *)
      val spsB = Vector.foldr (params, SPSBinds.empty, fn (r,spsB) => 
        SPSBinds.add spsB r (SPS.newColonArrow
          (TyD.makeTvar $ Tyvar.newNoname {equality=false},
           RelType.newVar $ RelTyvar.new ())))
      (* Loop over map generating type constraints *)
      val (cs,sortOp) = Vector.fold (map, 
        (Constraints.empty, NONE), 
        fn ((patop,rterm),(csacc,(relTySOp : SPS.t option))) => 
          case (patop,rterm) of 
            (NONE, RelLang.Star ie) =>
            let
              val _ = case relTySOp of NONE => ()
                | SOME _ => Error.bug "Impossible case star"
              val baseR = case ie of RelLang.Relation r => r
                | RelLang.Inst {rel,...} => rel
                | _ => raise (Fail "Star over rel param not allowed")
              val {ty = pTyS,...} = RE.find re baseR 
                handle (RE.RelNotFound _) => raise (Fail $ "Ind of \
                  \unknown relation: " ^ (RelId.toString baseR))
              val domainTyD = ProjTypeScheme.domain pTyS
              val (cs,relTy) = typeSynthIEApp (re, spsB, TyDB.empty, 
                ie, domainTyD)
              val sort = SPS.newColonArrow (domainTyD, relTy)
            in
              (* types of inductive and simple versions match *)
              (cs,SOME sort)
            end
          | (SOME (Pat.Con (con,valpatop)), RelLang.Expr rexpr) => 
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
              val sort = SPS.newColonArrow (datTyD, relTy)
              val cs = case relTySOp of NONE => cs 
                | SOME (SPS.ColonArrow (d,r)) => (assert (TyD.sameType
                    (datTyD,d), "Relation domains differ in case \
                      \branches");
                    Constraints.newRCstr cs $ RelTyC.new (relTy,r))
            in
              (cs,SOME sort)
            end
          | (SOME (Pat.Value v), RelLang.Expr rexpr) =>
              raise (Fail "unimpl"))
      (*  sortOp is the result of looping over the map.  *)
      val initSort = case sortOp of SOME sort => sort 
        | NONE => raise (Fail "impossible case of sort")
      (* Solve constraints. tyvar constraints are completely solved.
       * reltyvar constraints can contain unsolvable residue.
       *)
      val {tysolop,sortsol,residuecs} = Constraints.solve cs
      (* Correction for params which are wrongly assigned colonarrow
       * sorts
       *)
      val {yes=nullable,no} = Vector.partition (tysolop, 
        fn (tyvar,tydop) => case tydop of None => true | _ => false)
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
       * Expand inductive relations defined with star.
       * Resultant map is a reldesc map.
       *)
      val map2 = (Vector.concatV o Vector.map) (map1, fn (patop,rterm) =>
        case (patop,rterm) of 
          (SOME pat,RelLang.Expr rexpr) => Vector.new1 (pat,rexpr)
        | (NONE, RelLang.Star ie) => 
          let
            val (relId,args) = case ie of 
                RelLang.Relation rid => (rid, Vector.new0 ())
              | RelLang.Relvar _ => raise (Fail "Star of relvar.\n")
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
            Vector.map (map', fn (pat,rexpr) => case pat of 
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
          end
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
      (*
      val fullVE = Vector.fold (typespecs, refinedVE, 
        fn (TypeSpec.T (f,refTy),ve) => VE.add ve (f,RefTyS.generalize 
          (Vector.new0 (), refTy)))
      *)
    in
      (initialVE,elabRE)
    end
end

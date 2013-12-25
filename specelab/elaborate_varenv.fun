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
  structure SPS = SimpleProjSort
  structure SPSBinds = ApplicativeMap (structure Key = RelVar
                                       structure Value = SPS)
  structure RelTyC = RelTyConstraint

  val assert = Control.assert
  val leneq = Control.leneq
  fun $ (f,arg) = f arg
  infixr 5 $

  structure Constraints =
  struct
    structure TyDConstraint =
    struct
      datatype t = Eq of Tyvar.t * TyD.t option
      type sol = Tyvar.t * TyD.t option
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

  (*
  fun addRelToConTy (ve: VE.t) (con,valop,rexpr) (id:RelLang.RelId.t) =
    let
      val convid = Var.fromString (Con.toString con)
      val substs = case valop of NONE => Vector.fromList []
        | SOME vals => Vector.map (unifyConArgs ve con vals, 
          fn (cvar,var,_,_) => (cvar,var))
      val rexpr' = RelLang.applySubsts substs rexpr
      val newref = fn var => RP.Eq (RelLang.app(id,var),rexpr')
      val RefTyS.T {tyvars,refty} = VE.find ve convid
        handle (VE.VarNotFound v) => Error.bug ("Could not find constructor "
          ^ (Var.toString convid) ^ " in varenv\n")
      val annotConTy = case refty of
          RefTy.Base (bv,tyd,pred) => RefTy.Base (bv,tyd, 
            Predicate.conjR (pred,newref bv))
        | RefTy.Arrow (arg,RefTy.Base (bv,tyd,pred)) => RefTy.Arrow(arg,
            RefTy.Base (bv,tyd, Predicate.conjR (pred,newref bv)))
        | _ => raise (Fail "Constructor type is neither base not arrow")
      val newTyS = RefTyS.generalize (tyvars,annotConTy)
    in
      VE.add (VE.remove ve convid) (convid,newTyS)
    end
  *)

  fun typeSynthInstExpr (re, spsB, ie) : (Constraints.t * 
    SPS.t) = raise (Fail "Unimpl")
  (*
   * Synthesizes the type of rexpr in the given relational env.
   * Rel Env is constructed during elaboration, hence this function
   * is also part of elaboration.
   *)
  fun typeSynthRExpr (re,spsB,tyDB,rexpr) : (Constraints.t *
      RelType.t) = raise (Fail "Unimpl")
    (*let
      open RelLang
      open RelType
      val typeSynthRExpr = fn expr => typeSynthRExpr (re,tyDB,expr)
      fun typeSynthRElem elem = case elem of
          Int i => TyD.makeTconstr (Tycon.intInf,[])
        | Bool b => TyD.makeTconstr (Tycon.bool,[])
        | Var v => TyDBinds.find tyDB v 
    in
      case rexpr of 
        T elemvec => Tuple $ Vector.map (elemvec, typeSynthRElem)
      | X (e1,e2) => crossPrdType (typeSynthRExpr e1, typeSynthRExpr e2)
      | U (e1,e2) => unionType (typeSynthRExpr e1, typeSynthRExpr e2)
      | R (relId,arg) => 
        let
          val relName = RelId.toString relId
          val argTy = TyDBinds.find tyDB arg
          val tyds = Vector.fromList (case argTy of 
              TyD.Tconstr (_,tyds) => tyds
            | _ => Error.bug "Instantiating relation on a variable\
              \ of non-algebraic datatype")
          val {ty = relTyS,...} = RE.find re relId handle 
            RE.RelNotFound _ => Error.bug ("Instantiating unknown \
              \relation "^relName)
          val Tuple formalTyDs = RelTyS.instantiate (relTyS,tyds) 
          val _ = assert (TyD.sameType (argTy, Vector.sub 
            (formalTyDs,0)), "Type of formal and actual arguments \
              \does not match for relation "^relName)
          val instType = Tuple $ Vector.dropPrefix (formalTyDs,1)
        in
          instType
        end
    end*)

  fun projTypeScheme ve re {params,map} : ProjTypeScheme.t = 
    let
      (* Initially, assign colonarrow types to all params *)
      val spsB = Vector.foldr (params, SPSBinds.empty, fn (r,spsB) => 
        SPSBinds.add spsB r (SPS.newColonArrow
          (TyD.makeTvar $ Tyvar.newNoname {equality=false},
           RelType.newVar $ RelTyvar.new ())))
      (* Loop over map generating type constraints *)
      val spsnone : SPS.t option = NONE
      val (cs,sortOp) = Vector.fold (map, 
        (Constraints.empty, spsnone), 
        fn ((patop,rterm),(csacc,(relTySOp : SPS.t option))) => 
          case (patop,rterm) of 
            (NONE, RelLang.Star ie) =>
            let
              val _ = case relTySOp of NONE => ()
                | SOME _ => Error.bug "Impossible case star"
              val (cs,ty) = typeSynthInstExpr (re, spsB, ie)
            in
              (* types of inductive and simple versions match *)
              (cs,SOME ty)
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
                | SOME sort' => Constraints.newRCstr cs $
                    SPS.unify (sort,sort')
            in
              (cs,SOME sort)
            end
          | (SOME (Pat.Value v), RelLang.Expr rexpr) =>
              raise (Fail "unimpl"))
      (*
       * initSort is a result of SPS's assigned to params, and
       * dattyd
       *)
      val initSort = case sortOp of SOME sort => sort 
        | NONE => raise (Fail "impossible case of sort")
      (*
       * Solve constraints. tyvar constraints are completely solved.
       * reltyvar constraints can contain unsolvable residue.
       *)
      val {tysolop,sortsol,residuecs} = Constraints.solve cs
      (* 
       * Correction for params which are wrongly assigned colonarrow
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
            val sr = StructuralRelation.new {id = relId, 
                params = params, map = mapToSRMap map}
            val map' = srMapToMap $ StructuralRelation.instantiate sr args 
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
      (*
      val refinedVE = Vector.fold (RE.toVector elabRE, initialVE, 
        fn ((id,{ty,map}),ve) => Vector.fold (map, ve, 
          fn (conPatBind,ve) => addRelToConTy ve conPatBind id))
      val fullVE = Vector.fold (typespecs, refinedVE, 
        fn (TypeSpec.T (f,refTy),ve) => VE.add ve (f,RefTyS.generalize 
          (Vector.new0 (), refTy)))
      *)
    in
      (initialVE,elabRE)
    end
end

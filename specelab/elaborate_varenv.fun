functor ElaborateVarEnv (S : ELABORATE_VAR_ENV_STRUCTS) : ELABORATE_VAR_ENV = 
struct
  open S
  open SpecLang
  open ANormalCoreML
  structure VE = VarEnv (open SpecLang)
  structure RE = RelEnv (open SpecLang)
  structure TyD = TypeDesc
  structure RefTy = RefinementType
  structure RefTyS = RefinementTypeScheme
  structure RP = Predicate.RelPredicate

  val assert = Control.assert

  fun elabDatBind (ve : VE.t) {cons,tyvars,tycon} =
    let
      val destTyD = TyD.makeTconstr (tycon, 
        Vector.toListMap (tyvars, TyD.makeTvar))
      val elabCon = fn ({arg,con},ve) =>
        let
          val vid = Var.fromString (Con.toString con)
          val conTyD = case arg of 
              NONE => destTyD
            | SOME argTy => TyD.makeTarrow (Type.toMyType argTy, destTyD)
          val conTyS = RefTyS.generalize (tyvars, RefTy.fromTyD conTyD)
        in
          VE.add ve (vid,conTyS)
        end
    in
      Vector.fold (cons, ve, elabCon)
    end

  fun unifyConArgs (ve : VE.t) (con : Con.t) (vars : Var.t vector) =
    let
      val conTy = RefTyS.specialize (VE.find ve 
        (Var.fromString(Con.toString con)))
      open RefTy
    in
      case conTy of 
        Base (bv,_,_) => (assert(Vector.isEmpty vars); Vector.fromList [])
      | Arrow (Base (bv,argTyD,_),Base (_,datTyD,_)) => 
          (assert (Vector.length vars = 1);
          Vector.map (vars,fn (var) => 
            (bv, var, TyD.sametype (argTyD,datTyD))))
      | Arrow (Tuple tv, Base (_,datTyD,_)) =>
          (assert (Vector.length tv = Vector.length vars);
         Vector.map2 (tv,vars,fn (Base (bv,argTyD,_), var) =>
            (bv, var, TyD.sametype (argTyD,datTyD))))
      | _ => raise (Fail "Could not unify and determine rec args")
    end

  fun addRelToConTy (ve: VE.t) (con,valop,rexpr) (id:RelLang.RelId.t) =
    let
      val convid = Var.fromString (Con.toString con)
      val substs = case valop of NONE => Vector.fromList []
        | SOME vals => Vector.map (unifyConArgs ve con vals, 
          fn (cvar,var,_) => (cvar,var))
      val rexpr' = RelLang.applySubsts substs rexpr
      val newref = fn var => RP.Eq (RelLang.app(id,var),rexpr')
      val RefTyS.T {tyvars,refty} = VE.find ve convid
      val annotConTy = case refty of
          RefTy.Base (bv,tyd,pred) => RefTy.Base (bv,tyd, 
            Predicate.conjR (pred,newref bv))
        | RefTy.Arrow (_,RefTy.Base (bv,tyd,pred)) => RefTy.Base (bv,tyd, 
            Predicate.conjR (pred,newref bv))
        | _ => raise (Fail "Constructor type is neither base not arrow")
      val newTyS = RefTyS.generalize (tyvars,annotConTy)
    in
      VE.add ve (convid,newTyS)
    end

  fun elabSRBind (re: RE.t)(ve : VE.t) {id,ty,map} =
    let
      (* types of inductive and simple versions match *)
      val ty' = ty 
      val map' = (Vector.concatV o Vector.map) (map, fn (con,valop,rterm) =>
        case rterm of 
          RelLang.Expr rexpr => Vector.fromList [(con,valop,rexpr)]
        | RelLang.Star relId => 
          let
            val {ty,map} = RE.find re relId 
              handle RE.RelNotFound => raise (Fail "Ind of unknown relation")
          in
            Vector.map (map, fn (con,valop,rexpr) => case valop of 
                NONE => (con,valop,rexpr)
              | SOME vars => 
                let
                  val recvars = Vector.map ((#yes o Vector.partition) 
                    (unifyConArgs ve con vars, fn (cvar,var,isrec) => isrec),
                      fn (cvar,var,isrec) => cvar)
                  val recRApps = Vector.map (recvars, fn var => 
                    RelLang.app (id,var))
                  val recRAppsUnion = Vector.fold (recRApps,
                    RelLang.emptyexpr(), RelLang.union)
                  val rexpr' = RelLang.union (rexpr, recRAppsUnion)
                in
                  (con,valop,rexpr')
                end)
          end)
    in
      RE.add re (id,{ty=ty',map=map'})
    end

  fun elaborate (Program.T {decs=decs}) (RelSpec.T {reldecs, ...}) =
    let
      val initialVE = Vector.fold (decs,VE.empty,fn (dec,ve) =>
        case dec of Dec.Datatype datbinds => Vector.fold (datbinds, ve,
          fn (datbind,ve)   => elabDatBind ve datbind) 
          | _ => ve)
      val elabRE = Vector.fold (reldecs, RE.empty, 
        fn(StructuralRelation.T srbind,re) => elabSRBind re initialVE srbind)
      val refinedVE = Vector.fold (RE.toVector elabRE, initialVE, 
        fn ((id,{ty,map}),ve) => Vector.fold (map, ve, 
          fn (conPatBind,ve) => addRelToConTy ve conPatBind id))
    in
      refinedVE
    end
end

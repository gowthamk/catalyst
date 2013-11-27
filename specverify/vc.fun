functor VerificationCondition (S : VERIFICATION_CONDITION_STRUCTS)
  : VERIFICATION_CONDITION =
struct
  open S
  structure P = Predicate
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure RefTy = RefinementType
  structure RefTyS = RefinementTypeScheme
  structure RelTy = RelLang.RelType
  structure RelTyS = RelLang.RelTypeScheme
  structure RI = RelLang.RelId
  structure TyD = TypeDesc
  structure Env = TyDBinds
  structure L = Layout

  type tydbind = Var.t * TyD.t
  type tydbinds = tydbind vector

  datatype simple_pred = True
                       |  Base of BP.t 
                       |  Rel of RP.t

  datatype vc_pred =  Simple of simple_pred
                   |  Conj of simple_pred vector

  datatype t = T of tydbinds * vc_pred* simple_pred
  
  val assert = Control.assert
  fun $ (f,arg) = f arg
  infixr 5 $
  fun vectorAppend (vec,e) = Vector.concat [vec,Vector.new1 e]
  fun vectorPrepend (e,vec) = Vector.concat [Vector.new1 e,vec]
  fun vectorFoldrFoldr (vec1,vec2,acc,f) = Vector.foldr (vec1,acc,
    fn (el1,acc) => Vector.foldr (vec2,acc,fn (el2,acc) => f (el1,el2,acc)))

  fun conj (p1 : vc_pred,p2 : vc_pred) : vc_pred = case (p1,p2) of 
      (Simple True,_) => p2
    | (_, Simple True) => p1
    | (Simple sp1,Simple sp2) => Conj (Vector.new2 (sp1,sp2))
    | (Simple sp1,Conj spv) => Conj $ vectorPrepend (sp1,spv)
    | (Conj spv,Simple sp2) => Conj $ vectorAppend (spv,sp2)
    | (Conj spv1,Conj spv2) => Conj $ Vector.concat [spv1,spv2]
  
  fun coercePTtoT (pt:P.t) : vc_pred = case pt of
      P.True => Simple True
    | P.Base p => Simple $ Base p
    | P.Rel p => Simple $ Rel p
    | P.Conj (p1,p2) => 
        let
          val t1 = coercePTtoT p1
          val t2 = coercePTtoT p2
        in
          conj (t1,t2)
        end
    | _ => Error.bug "Cannot coerce PT to T"

  fun truee () : vc_pred = Simple True

  (*
   * join-order(vc,vc1,vc2) : binds = binds1@binds2
   *                          envP = envP1 /\ envP2
   *)
  fun joinVCs ((binds1,envP1),(binds2,envP2)) : (tydbinds * vc_pred) =
    (Vector.concat [binds1,binds2],conj (envP1,envP2))

  (*
   * forall vc1 in vcs1 and vc2 in vcs2, vc is in vcs s.t
   * join-order (vc,vc1,vc2)
   *)
  fun join (vcs1,vcs2) = 
    case (Vector.length vcs1, Vector.length vcs2) of 
      (0,_) => vcs2
    | (_,0) => vcs1
    | _ => 
      let
        val vcs = (Vector.fromList o vectorFoldrFoldr) (vcs1,vcs2,[], 
          fn (vc1,vc2,acc) => (joinVCs (vc1,vc2))::acc)
      in
        vcs
      end

  fun havocPred (pred : P.t) : (tydbinds*vc_pred) vector =
    let
      fun trivialAns () = Vector.new1 (Vector.new0(),coercePTtoT pred)
    in
      case pred of
        P.Exists (tyDB,p) => 
          let
            val mybinds = TyDBinds.toVector tyDB
            val vcs = havocPred p
          in
            Vector.map (vcs, fn (binds,envP) =>
              (Vector.concat [mybinds,binds],envP))
          end
      | P.Conj (p1,p2) =>
          let
            val vcs1 = havocPred p1
            val vcs2 = havocPred p2
          in
            (* conj is a join point *)
            join (vcs1,vcs2)
          end
      | P.Disj (p1,p2) => Vector.concat [havocPred p1,
          havocPred p2]
      | _ => trivialAns ()
    end
        
  fun havocTyBind (v : Var.t,refTy : RefTy.t) : (tydbinds*vc_pred) vector =
    let
      open RefTy
    in
      case refTy of
        Base (bv,td,pred) => 
          let
            val pred' = P.applySubst (v,bv) pred
            val vcs = havocPred pred'
            val mybind = (v,td)
          in
            Vector.map (vcs,fn (binds,envP) => 
              (vectorAppend (binds,mybind),envP))
          end
        (* Bindings for tuples/functions not needed *)
      | _ => Vector.new0 ()
    end

  fun havocVE (ve : VE.t) : (tydbinds*vc_pred) vector =
    let
      (*
       * Remove polymorphic functions and constructors
       *)
      val vevec = Vector.keepAllMap (VE.toVector ve,
        fn (v,RefTyS.T{tyvars,refty}) => case Vector.length tyvars of
            0 =>  SOME (v,refty)
          | _ => NONE)
      val init = Vector.new1 (Vector.new0 (),truee())
    in
      Vector.fold (vevec,init,fn (tyBind,vcs1) => 
          join (vcs1,havocTyBind tyBind))
    end

  fun fromTypeCheck (ve, subTy, supTy) : t vector = 
    let
      open RefTy
    in
      case (subTy,supTy) of
        (Base (v1,t1,p1), Base (v2,t2,p2)) => 
          let
            (*
             * First, make sure that base types are same.
             *)
            val _ = assert (TyD.sameType (t1,t2), (TyD.toString t1)
              ^" is not sub-type of "^(TyD.toString t2))
            (*
             * Second, substitute actuals for formals in p2
             *)
            val p2 = P.applySubst (v1,v2) p2
            (*
             *val _ = print "AnteP: "
             *val _ = L.print (P.layout p1,print)
             *val _ = print "\n"
             *)
            (*
             * Third, add base type of actuals to env
             *)
            val ve = VE.add ve (v1,RefTyS.generalize (Vector.new0 (),
              RefTy.fromTyD t1))
            val envVCs = fn _ => havocVE ve
            val anteVCs = fn _ => havocPred p1
            val vcs = fn _ => join (envVCs (),anteVCs ())
            val conseqPs = fn _ => case coercePTtoT p2 of
                Conj spv => spv | Simple sp => Vector.new1 (sp)
          in
            case p2 of P.True => Vector.new0()
              | _ => Vector.fromList $ vectorFoldrFoldr 
                (vcs(), conseqPs(), [],
                  fn ((tybinds,anteP),conseqP,vcacc) => 
                    (T (tybinds,anteP,conseqP))::vcacc)
          end
      | (Tuple t1v,Tuple t2v) => 
          (*
           * Unimpl: records
           *)
          (Vector.concatV o Vector.map2) (t1v,t2v, 
            fn ((v1,t1),(v2,t2)) => fromTypeCheck (ve,t1,t2))
      | (Arrow ((arg1,t11),t12),Arrow ((arg2,t21),t22)) => 
          let
            val vcs1 = fromTypeCheck (ve,t21,t11)
            (*
             * Typecheck results modulo argvar
             *)
            val t12' = RefTy.applySubsts (Vector.new1 (arg2,arg1)) t12
            val vcs2 = fromTypeCheck (ve,t12',t22)
          in
            Vector.concat [vcs1, vcs2]
          end 
    end

  datatype rinst = RInst of RelLang.RelId.t * TypeDesc.t vector

  structure RelInstTable : APPLICATIVE_MAP where
    type Key.t = rinst and type Value.t = RelLang.RelId.t =
  struct
    structure Key = 
    struct
      type t = rinst
      val layout = fn _ => L.empty
      val idStrEq = fn (id1,id2) => (RI.toString id1 = RI.toString id2)
      fun equal (RInst (id1,tyds1), RInst (id2,tyds2)) =
        let
          val eq = (idStrEq (id1,id2)) andalso
              (Vector.length tyds1 = Vector.length tyds2) andalso
              (Vector.forall2 (tyds1,tyds2, TyD.sameType))
        in
          eq
        end
        
    end
    structure Map = ApplicativeMap (structure Key = Key
                                   structure Value = RelLang.RelId)
    open Map
  end

  fun elaborate (re,vc) =
    let
      val T (tydbinds,anteP,conseqP) = vc

      val tyDB = Vector.fold (tydbinds,TyDBinds.empty, 
        fn ((v,tyd),tyDB) => TyDBinds.add tyDB v tyd)

      val count = ref 0
      val genSym = fn idbase => 
        let
          val symbase = (*"_"^*)(RI.toString idbase)
          val id = symbase ^ (Int.toString (!count))
          val _ = count := !count + 1
        in
          RI.fromString id 
      end
      val inv = fn (x,y) => (y,x)
      fun mapFoldTuple b f (x,y) =
        ((fn (b',x') => 
            ((fn (b'',y') => (b'',(x',y'))) o (f b')) y) 
          o (f b)) x 
      fun mapSnd f (x,y) = (x,f y)

      fun elabRExpr (tab:RelInstTable.t) rexpr =  
        let
          fun getSymForRInst rinst = 
            (SOME $ RelInstTable.find tab (RInst rinst)) 
              handle RelInstTable.KeyNotFound _ => NONE
          fun addSymForRInst rinst rid =
            RelInstTable.add tab (RInst rinst) rid
          val mapper = mapFoldTuple tab elabRExpr
          fun tyArgsinTypeOf (v:Var.t) =
            (case TyDBinds.find tyDB v of
              TyD.Tconstr (tycon,targs) => Vector.fromList targs
            | _ => Error.bug ("Relation instantiated over variable\
              \ of non-algebraic datatype")) 
            handle TyDBinds.KeyNotFound _ => Error.bug ("Type of\
              \ variable "^(Var.toString v)^" not found in TyDBinds")
        in
          case rexpr of
            RelLang.T _ => (tab,rexpr)
          | RelLang.X t => mapSnd RelLang.X (mapper t)
          | RelLang.U t => mapSnd RelLang.U (mapper t)
          | RelLang.R (relId,v) => 
            let
              val rinst = (relId,tyArgsinTypeOf v)
            in
              case getSymForRInst rinst of 
                SOME relId' => (tab,RelLang.R (relId',v))
              | NONE => (fn r' => (addSymForRInst rinst r', 
                  RelLang.R (r',v))) (genSym relId)
            end
        end

      fun elabRPred (tab : RelInstTable.t) rpred = case rpred of
          RP.Eq t => mapSnd RP.Eq (mapFoldTuple tab elabRExpr t)
        | RP.Sub t => mapSnd RP.Sub (mapFoldTuple tab elabRExpr t)
        | RP.SubEq t => mapSnd RP.SubEq (mapFoldTuple tab elabRExpr t)

      fun elabSimplePred (rinstTab : RelInstTable.t) sp = 
        case sp of
          Rel rpred => mapSnd Rel (elabRPred rinstTab rpred)
        | _ => (rinstTab,sp)

      fun elabVCPred (rinstTab : RelInstTable.t) (vcpred:vc_pred) :
        (RelInstTable.t*vc_pred) = 
        case vcpred of
          Simple sp  => mapSnd Simple (elabSimplePred rinstTab sp)
        | Conj spvec => mapSnd Conj ((inv o Vector.mapAndFold) 
           (spvec, rinstTab, fn (sp,rt) => inv $ elabSimplePred rt sp))

      val (rinstTab,anteP') = elabVCPred RelInstTable.empty anteP
      val (rinstTab,conseqP') = elabSimplePred rinstTab conseqP

      val newtydbinds = Vector.map (RelInstTable.toVector rinstTab,
        fn (RInst (relId,tydvec),relId') =>
          let
            val {ty,map} = RE.find re relId handle RE.RelNotFound _ =>
              Error.bug ("Unknown Relation: "^(RelLang.RelId.toString relId))
            val RelTy.Tuple tydvec = RelTyS.instantiate (ty,tydvec)
            val boolTyD = TyD.makeTconstr (Tycon.bool,[])
            val relArgTyd = TyD.Trecord $ Record.tuple tydvec
            val relTyD = TyD.makeTarrow (relArgTyd,boolTyD)
            val relvid = Var.fromString $ RI.toString relId'
          in
            (relvid,relTyD)
          end)

      val tydbinds' = Vector.concat [tydbinds,newtydbinds]
    in
      T (tydbinds',anteP',conseqP')
    end

  fun layout (vcs : t vector) =
    let
      fun laytTyDBinds tybinds = L.vector (Vector.map (tybinds,
        fn (v,tyd) => L.str ((Var.toString v) ^ " : " ^ 
          (TyD.toString tyd))))

      fun laytSimplePred sp = case sp of 
          True => L.str "true"
        | Base bp => L.str $ BP.toString bp
        | Rel rp => L.str $ RP.toString rp

      fun laytVCPred vcpred = case vcpred of 
          Simple p => laytSimplePred p
        | Conj simpv => L.align $ Vector.toListMap (simpv,
            laytSimplePred)

      fun layoutVC (T (tybinds,vcp,sp)) = 
        Pretty.nest ("bindings",laytTyDBinds tybinds,
          L.align [
            L.indent(laytVCPred vcp,3),
            L.str "=>",
            L.indent (laytSimplePred sp,3)])
    in
      L.align $ Vector.toListMap (vcs, layoutVC)
    end

  fun layouts (vcs,output) =
    (output $ L.str "Verification Conditions:\n" ; output $ layout vcs)
    
end

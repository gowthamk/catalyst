functor VerificationCondition (S : VERIFICATION_CONDITION_STRUCTS)
  : VERIFICATION_CONDITION =
struct
  open S
  structure P = Predicate
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure RefTy = RefinementType
  structure RefTyS = RefinementTypeScheme
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

  datatype t = T of tydbinds * vc_pred* vc_pred

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
      val vevec = Vector.map (VE.toVector ve,
        fn (v,refTyS) => (v,RefTyS.specialize refTyS))
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
             * First, substitute actuals for formals in p2
             *)
            val p2 = P.applySubst (v1,v2) p2
            val envVCs = fn _ => havocVE ve
            val anteVCs = fn _ => havocPred p1
            val vcs = fn _ => join (envVCs (),anteVCs ())
            val conseqP = fn _ => coercePTtoT p2
          in
            case p2 of P.True => Vector.new0()
              | _ => Vector.map (vcs(), fn (tybinds,anteP) => 
                T (tybinds,anteP,conseqP()))
          end
      | (Tuple t1v,Tuple t2v) => (Vector.concatV o Vector.map2) 
          (t1v,t2v, fn (t1,t2) => fromTypeCheck (ve,t1,t2))
      | (Arrow (t11,t12),Arrow (t21,t22)) => Vector.concat
          [fromTypeCheck (ve,t21,t11), fromTypeCheck (ve,t12,t22)]
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

      fun layoutVC (T (tybinds,vcp1,vcp2)) = 
        Pretty.nest ("bindings",laytTyDBinds tybinds,
          L.align [
            L.indent(laytVCPred vcp1,3),
            L.str "=>",
            L.indent (laytVCPred vcp2,3)])
    in
      L.align $ Vector.toListMap (vcs, layoutVC)
    end

  fun layouts (vcs,output) =
    (output $ L.str "Verification Conditions:\n" ; output $ layout vcs)
    
end

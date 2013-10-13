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

  type tydbind = Var.t * TyD.t
  type tydbinds = tydbind vector

  datatype simple_pred = True
                       |  Base of BP.t 
                       |  Rel of RP.t
                       |  Conj of simple_pred * simple_pred 

  datatype t = T of tydbinds * simple_pred * simple_pred

  fun $ (f,arg) = f arg
  infixr 5 $
  fun vectorAppend (vec,e) = Vector.concat [vec,Vector.new1 e]

  fun coercePTtoT pt = case pt of
      P.True => True
    | P.Base p => Base p
    | P.Rel p => Rel p
    | P.Conj (p1,p2) => Conj (coercePTtoT p1, coercePTtoT p2)
    | _ => Error.bug "Cannot coerce PT to T"

  fun conj (p1,p2) = Conj (p1,p2)

  fun truee () = True

  (*
   * join-order(vc,vc1,vc2) : binds = binds1@binds2
   *                          envP = envP1 /\ envP2
   *)
  fun joinVCs ((binds1,envP1),(binds2,envP2)) : (tydbinds * simple_pred) =
    (Vector.concat [binds1,binds2],conj (envP1,envP2))

  (*
   * forall vc1 in vcs1 and vc2 in vcs2, vc is in vcs s.t
   * join-order (vc,vc1,vc2)
   *)
  fun join (vcs1,vcs2) = 
    case (Vector.length vcs1, Vector.length vcs2) of 
      (0,_) => vcs2
    | (_,0) => vcs1
    | _ => (Vector.fromList o Vector.foldr2) (vcs1,vcs2,[], 
      fn (vc1,vc2,acc) => (joinVCs (vc1,vc2))::acc)

  fun havocPred (pred : P.t) : (tydbinds*simple_pred) vector =
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
        
  fun havocTyBind (v : Var.t,refTy : RefTy.t) : (tydbinds*simple_pred) vector =
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

  fun havocVE (ve : VE.t) : (tydbinds*simple_pred) vector =
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
      val envVCs = havocVE ve
      open RefTy
    in
      case (subTy,supTy) of
        (Base (v1,t1,p1), Base (v2,t2,p2)) => 
          let
            val anteVCs = havocPred p1
            val vcs = join (envVCs,anteVCs)
            val conseqP = coercePTtoT p2
          in
            Vector.map (vcs, fn (tybinds,anteP) => 
              T (tybinds,anteP,conseqP))
          end
      | (Tuple t1v,Tuple t2v) => (Vector.concatV o Vector.map2) 
          (t1v,t2v, fn (t1,t2) => fromTypeCheck (ve,t1,t2))
      | (Arrow (t11,t12),Arrow (t21,t22)) => Vector.concat
          [fromTypeCheck (ve,t21,t11), fromTypeCheck (ve,t12,t22)]
    end
    
end

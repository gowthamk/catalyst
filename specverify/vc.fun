functor VerificationCondition (S : VERIFICATION_CONDITION_STRUCTS)
  : VERIFICATION_CONDITION =
struct
  open S
  structure P = Predicate

  structure BP = Predicate.BasePredicate

  structure RP = Predicate.RelPredicate

  structure RefTy = RefinementType

  structure Env = TyDBinds

  datatype t = T of TyDBinds.t * P.t * P.t

  type tydbind = Var.t * TyD.t
  type tydbinds = tydbind vector

  fun $ (f,arg) = f arg
  infixr 5 $
  fun vectorAppend (vec,e) = Vector.concat [vec,Vector.new1 e]

  fun joinVCs ((binds1,envP1),(binds2,envP2)) : (tydbinds * P.t) =
    (Vector.concat [binds1,binds2],conj (envP1,envP2))

  fun join (vcs1,vcs2) = 
    case (Vector.length vcs1, Vector.length vcs2) of 
      (0,_) => vcs2
    | (_,0) => vcs1
    | _ => (Vector.fromList o Vector.foldr2) (vcs1,vcs2,[], 
      fn (vc1,vc2,acc) => (joinVCs (vc1,vc2))::acc)

  fun havocPred (pred : P.t) : (tydbinds*P.t) vector =
    let
      open Predicate
      fun trivialAns (p : P.t) = (Vector.new0(),p)
    in
      case pred of
        True => trivialAns pred
      | Base _ => trivialAns pred
      | Rel _ => trivialAns pred
      | Exists (tyDB,p) => 
          let
            val mybinds = TyDBinds.toVector tyDB
            val vcs = havocPred p
          in
            Vector.map (vcs, fn (binds,envP) =>
              Vector.concat [mybinds,binds],envP)
          end
      | Conj (p1,p2) =>
          let
            val vcs1 = havocPred p1
            val vcs2 = havocPred p2
          in
            (* conj is a join point *)
            join (vcs1,vcs2)
          end
      | Disj (p1,p2) => Vector.concatV (havocPred p1,
          havocPred p2)
    end
        
  fun havocTyBind (v : Var.t,refTy : RefTy.t) : (tybinds*P.t) vector =
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
      | _ => Vector.new0 ()
    end

  fun havocVE (ve : VE.t) : (tybinds*P.t) vector =
    let
      val vevec = Vector.map (VE.toVector ve,
        fn (v,refTyS) => (v,RefTyS.specialize refTyS))
      val init = Vector.new1 (Vector.new0 (),P.truee())
    in
      Vector.fold (vevec,[],fn (tyBind,vcs1) => 
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
            val conseqP = p2
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

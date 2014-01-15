functor VerificationCondition (S : VERIFICATION_CONDITION_STRUCTS)
  : VERIFICATION_CONDITION =
struct
  open S
  structure P = Predicate
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure RefTy = RefinementType
  structure RefTyS = RefinementTypeScheme
  structure RefSS = RefinementSortScheme
  structure RelTy = RelType
  structure RI = RelId
  structure TyD = TypeDesc
  structure Env = TyDBinds
  structure L = Layout

  type tydbind = Var.t * TyD.t
  type tydbinds = tydbind vector

  type sol = (RelVar.t * RelLang.ieatom) vector
  
  datatype pred = True 
                | Base of BP.t
                | Rel of RP.t
                | Conj of pred vector
                | Forall of {rtyvs : RelTyvar.t vector,
                             cs : RelTyConstraint.t vector,
                             params : (RelVar.t * SimpleProjSort.t) vector
                             } * pred
  datatype t = T of tydbinds * pred * pred

  val assert = Control.assert
  fun $ (f,arg) = f arg
  infixr 5 $
  val allvcs = ref []
  fun vectorAppend (vec,e) = Vector.concat [vec,Vector.new1 e]
  fun vectorPrepend (e,vec) = Vector.concat [Vector.new1 e,vec]
  fun vectorFoldrFoldr (vec1,vec2,acc,f) = Vector.foldr (vec1,acc,
    fn (el1,acc) => Vector.foldr (vec2,acc,fn (el2,acc) => f (el1,el2,acc)))

  fun conj (p1 : pred,p2 : pred) : pred = case (p1,p2) of 
      (True,_) => p2
    | (_, True) => p1
    | (Conj spv1,Conj spv2) => Conj $ Vector.concat [spv1,spv2]
    | (sp1,Conj spv) => Conj $ vectorPrepend (sp1,spv)
    | (Conj spv,sp2) => Conj $ vectorAppend (spv,sp2)
    | (sp1,sp2) => Conj (Vector.new2 (sp1,sp2))

  fun coercePTtoPred (pt:P.t) : pred = case pt of
      P.True => True
    | P.Base p => Base p
    | P.Rel p => Rel p
    | _ => Error.bug "Cannot coerce PT to T"

  fun coercePredToPT (pred : pred) : P.t = case pred of
      True => P.True
    | Base p => P.Base p
    | Rel p => P.Rel p
    | Conj ps => Vector.fold (ps,P.True, fn (p,acc) =>
        P.Conj (coercePredToPT p,acc))
    | Forall (x,p) => P.Forall (x, coercePredToPT p)
    
  fun truee () : pred = True
  
  fun forallPred (refSS, pred) =
    let
      val RefSS.T {reltyvars = rs1, paramrefty = prt1, 
        constraints = cs1} = refSS 
      val ps1 = RefSS.typedParams prt1
    in
      case Vector.length ps1 of 0 => pred
      | _ => P.forall (rs1, cs1, ps1, pred)
    end

  (*
   * join-order(vc,vc1,vc2) : binds = binds1@binds2
   *                          envP = envP1 /\ envP2
   *)
  fun joinVCs ((binds1,envP1),(binds2,envP2)) : (tydbinds * pred) =
    (Vector.concat [binds1,binds2],conj (envP1,envP2))

  fun joinSols (sols1,sols2) =
    let
      fun disjointUnion (sol1,sol2) = 
        let
          val dom1 = Vector.toListMap (sol1,#1)
          val dom2 = Vector.toListMap (sol2,#1)
          val {areDisjoint, ...} = List.set {equals = RelVar.eq,
            layout = RelVar.layout}
          val _ = assert (areDisjoint (dom1,dom2), "Solution\
            \ domains not disjoint. Cannot join.")
        in
          Vector.concat [sol1,sol2]
        end
    in
      case (Vector.length sols1, Vector.length sols2) of
        (0,_) => sols2 | (_,0) => sols1 
      | _ => Vector.fromList $ vectorFoldrFoldr (sols1, sols2, [],
          fn (sol1,sol2,acc) => (disjointUnion (sol1,sol2))::acc)
    end

  fun havocPred (pred : P.t) : (tydbinds*pred) =
    let
      fun trivialAns () = (Vector.new0(),coercePTtoPred pred)
    in
      case pred of
        P.Exists (tyDB,p) => 
        let
          val mybinds = TyDBinds.toVector tyDB
          val (binds,envP) = havocPred p
        in
            (Vector.concat [mybinds,binds],envP)
        end
      | P.Conj (p1,p2) =>
        let
          val vc1 = havocPred p1
          val vc2 = havocPred p2
        in
          joinVCs (vc1,vc2)
        end
      | P.Forall (x,p) => 
        let
          val {rtyvs,cs,params} = x
          val (tydbinds,p') = havocPred p
        in
          case Vector.length params of
            0 => (tydbinds,p')
          | _ => (tydbinds, Forall (x,p'))
        end 
      | _ => trivialAns ()
    end

  fun havocSortBind (v : Var.t,refSS : RefSS.t) : (tydbinds*pred) =
    let
      val refTy = RefSS.toRefTy refSS
      open RefTy
      (* -- These functions duplicated from SpecVerify -- *)
      val newLongVar = fn (var,fld) => Var.fromString $
        (Var.toString var)^"."^(Var.toString fld)
      (*
       * Decomposes single tuple bind of form v ↦ {x0:T0,x1:T1} to
       * multiple binds : [v.x0 ↦ T0, v.x1 ↦ T1]
       *)
      fun decomposeTupleBind (tvar : Var.t, tty as RefTy.Tuple 
        refTyBinds) : (Var.t*RefTy.t) vector =
        let
          val bindss = Vector.map (refTyBinds, 
            fn (refTyBind as (_,refTy)) => 
              case refTy of 
                RefTy.Tuple _ => decomposeTupleBind refTyBind
              | _ => Vector.new1 refTyBind)
          val binds = Vector.map (Vector.concatV bindss, 
            fn (v,ty) => (newLongVar (tvar,v), ty))
        in
          binds
        end
    in
      case refTy of
        Base (bv,td,pred) => 
        let
          val pred' = forallPred (refSS,P.applySubst (v,bv) pred)
          val (binds,envP) = havocPred pred'
          val mybind = (v,td)
        in
            (vectorAppend (binds,mybind),envP)
        end
      | Tuple _ =>
        let
          val refTys =  decomposeTupleBind (v,refTy)
          val sortBinds = Vector.map (refTys, fn (v,refTy) => 
            (v, RefSS.substRefTy (refSS, refTy)))
        in  
          havocSortBindSeq sortBinds
        end
        (* Bindings for functions not needed *)
      | _ => (Vector.new0 (), truee ())
    end

  and havocSortBindSeq (sortBinds : (Var.t * RefSS.t) vector)
    : (tydbinds * pred) =
    Vector.fold (sortBinds, (Vector.new0 (),truee()),
      fn (sortBind,vcacc) => joinVCs (vcacc,havocSortBind sortBind))

  fun havocVE (ve : VE.t) : (tydbinds * pred) =
    let
      (*
       * Remove polymorphic functions and constructors
       *)
      val vevec = Vector.keepAllMap (VE.toVector ve,
        fn (v,RefTyS.T{tyvars,sortscheme}) => 
          case Vector.length tyvars of
            0 =>  SOME (v,sortscheme)
          | _ => NONE)
    in
      havocSortBindSeq vevec
    end

  fun layout (vcs : t vector) =
    let
      fun laytTyDBinds tybinds = L.vector (Vector.map (tybinds,
        fn (v,tyd) => L.str ((Var.toString v) ^ " : " ^ 
          (TyD.toString tyd))))

      fun laytPred pred = case pred of
          True => L.str "true"
        | Base bp => L.str $ BP.toString bp
        | Rel rp => L.str $ RP.toString rp
        | Conj ps => L.align $ Vector.toListMap (ps,
            laytPred)
        | Forall ({rtyvs,cs,params},t) => 
          let
            val rtyvlyt = L.vector $ Vector.map (rtyvs,fn rtyv =>
              L.str $ RelTyvar.toString rtyv)
            fun typedParamLyt (r,sprojty) = L.str $ 
              (RelVar.toString r) ^ " :: " ^
              (SimpleProjSort.toString sprojty)
            val paramslyt = L.vector $ Vector.map (params, typedParamLyt)
            val tylyt = laytPred t
          in
            Pretty.nest ("forall", 
              L.align[ 
                L.seq [rtyvlyt, L.str ". "],
                L.seq [paramslyt, L.str ". "]
                ], tylyt)
          end

      fun layoutVC (T (tybinds,vcp,sp)) = 
        Pretty.nest ("bindings",laytTyDBinds tybinds,
          L.align [
            laytPred vcp,
            L.str "=>",
            laytPred sp])
    in
      L.align $ Vector.toListMap (vcs, layoutVC)
    end

  fun layouts (vcs,output) =
    (output $ L.str "Verification Conditions:\n" ; output $ layout vcs)

  fun solveTypeCheck (ve, re, subSS, supSS) :
     sol vector =
    let
      val _ = print "VC Typecheck\n"
      (*
      val _ = print "Var Env:\n"
      val _ = Control.message (Control.Top, fn _ =>
        VE.layout ve)
      val _ = print "Rel Env:\n"
      val _ = Control.message (Control.Top, fn _ =>
        RE.layout re)
      *)
      val _ = print "--------------------------\n"
      val _ = Control.message (Control.Top, fn _ =>
        RefSS.layout subSS)
      val _ = print " <: "
      val _ = Control.message (Control.Top, fn _ =>
        RefSS.layout supSS)
      val _ = print "--------------------------\n"
      val subTy = RefSS.toRefTy subSS
      val supTy = RefSS.toRefTy supSS
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
             * Second, push generalized relvars in to predicate
             *)
            val p1 = forallPred (subSS, p1)
            (*
             * While substituting actuals for formals in p2
             *)
            val p2 = forallPred (supSS, P.applySubst (v1,v2) p2)
            (*
             * Third, add base type of actuals to env
             *)
            val ve = VE.add ve (v1,RefTyS.generalize (Vector.new0 (),
              RefSS.fromRefTy $ RefTy.fromTyD t1))
            val envVC = fn _ => havocVE ve
            val anteVC = fn _ => havocPred p1
            val vc = fn _ => joinVCs (envVC (),anteVC ())
            val conseqP = fn _ => havocPred p2
            val fullVC = case (vc (),conseqP ()) of 
              ((tydbinds,anteP),(tydbinds',conseqP)) =>
                (assert (Vector.isEmpty tydbinds', "Existential in\
                  \ consequent. Impossible."); 
                T (tydbinds, anteP, conseqP))
            val _ = List.push (allvcs,fullVC)
            val emptysol = Vector.new0 ()
          in
            emptysol
          end
      | (Tuple t1v,Tuple t2v) => Vector.new0 ()
      | (Arrow ((arg1,t11),t12),Arrow ((arg2,t21),t22)) => 
          let    
            (*
             * Typecheck results modulo argvar
             *)
            val t12' = RefTy.applySubsts (Vector.new1 (arg2,arg1)) t12
            val [s11,s12] = List.map ([t11,t12'], fn x => 
              RefSS.substRefTy (subSS,x))
            val [s21,s22] = List.map ([t21,t22], fn x => 
              RefSS.substRefTy (supSS,x))
            val sols1 = solveTypeCheck (ve,re,s21,s11)
            (*
             * Extend VE with tydbind for argvar
             *)
            val extendedVE = VE.add ve (arg2, RefTyS.fromRefTy 
              $ RefTy.fromTyD $ RefTy.toTyD t21)
            val sols2 = solveTypeCheck (extendedVE,re,s12,s22)
          in
            joinSols (sols1,sols2)
          end
    end

    fun printVCsToFile () =
      let
        val vcs = Vector.fromList $ List.rev $ !allvcs
        val _ = Control.saveToFile ({suffix = "newvcs"}, Control.No, 
                 vcs, Control.Layouts layouts)
      in
        ()
      end
end

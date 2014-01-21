functor VerificationCondition (S : VERIFICATION_CONDITION_STRUCTS)
  : VERIFICATION_CONDITION =
struct
  open S
  structure P = Predicate
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure PSS = ProjSortScheme
  structure PTS = ProjTypeScheme
  structure SPS = SimpleProjSort
  structure RelTyC = RelTyConstraint
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

  exception UnsolvableVC of string
  exception InvalidVC
  
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
  val fst = fn (x,y) => x
  val vcOf = fn x => T x
  fun $ (f,arg) = f arg
  infixr 5 $
  val (allvcs : t list ref) = ref []
  fun vectorAppend (vec,e) = Vector.concat [vec,Vector.new1 e]
  fun vectorPrepend (e,vec) = Vector.concat [Vector.new1 e,vec]
  fun vectorFoldrFoldr (vec1,vec2,acc,f) = Vector.foldr (vec1,acc,
    fn (el1,acc) => Vector.foldr (vec2,acc,fn (el2,acc) => f (el1,el2,acc)))
  val tyVarEq = fn (v1,v2) => (Tyvar.toString v1 = Tyvar.toString v2)
  fun isValidInst (eqs,els,isEq) = 
    let
      (*
       * Clear tautologies. This is a hack. Unimpl : remove.
       *)
      val eqs' = Vector.keepAll (eqs, fn (v,tyd) => case tyd of
        TyD.Tvar v' => not $ tyVarEq (v,v') | _ => true)
      val isGeneralized = fn x => Vector.exists (els, fn el =>
        isEq (x,el))
    in
      Vector.forall (eqs', fn (v,_) =>  isGeneralized v)
    end

  structure ElabVC =
  struct
    datatype simple_pred = True
                         |  Base of BP.t 
                         |  Rel of RP.t

    datatype vc_pred =  Simple of simple_pred
                     |  Conj of simple_pred vector

    datatype t = T of tydbinds * vc_pred* simple_pred 
  end

  fun conj (p1 : pred,p2 : pred) : pred = case (p1,p2) of 
      (True,_) => p2
    | (_, True) => p1
    | (Conj spv1,Conj spv2) => Conj $ Vector.concat [spv1,spv2]
    | (sp1,Conj spv) => Conj $ vectorPrepend (sp1,spv)
    | (Conj spv,sp2) => Conj $ vectorAppend (spv,sp2)
    | (sp1,sp2) => Conj (Vector.new2 (sp1,sp2))

  fun vcPredConj (p1 : ElabVC.vc_pred, p2 : ElabVC.vc_pred) =
    let 
      open ElabVC
    in
      case (p1,p2) of
        (Simple p1,Simple p2) => Conj $ Vector.new2 (p1,p2)
      | (Simple p, Conj spv) => Conj $ Vector.concat 
          [Vector.new1 p, spv]
      | (Conj spv, Simple p) => Conj $ Vector.concat 
          [Vector.new1 p, spv]
      | (Conj spv1, Conj spv2) => Conj $ Vector.concat [spv1,spv2]
    end

  fun coercePTtoPred (pt:P.t) : pred = case pt of
      P.True => True
    | P.Base p => Base p
    | P.Rel p => Rel p
    | P.Conj (p1,p2) => conj (coercePTtoPred p1,coercePTtoPred p2)
    | _ => Error.bug "Cannot coerce PT to pred"

  fun coercePredToPT (pred : pred) : P.t = case pred of
      True => P.True
    | Base p => P.Base p
    | Rel p => P.Rel p
    | Conj ps => Vector.fold (ps,P.True, fn (p,acc) =>
        P.Conj (coercePredToPT p,acc))
    | _ => Error.bug "Cannot coerce pred to PT"

  fun coercePredtoVCP (pred : pred) : ElabVC.vc_pred =
    case pred of
      True => ElabVC.Simple $ ElabVC.True
    | Base p => ElabVC.Simple $ ElabVC.Base p
    | Rel p => ElabVC.Simple $ ElabVC.Rel p
    | Conj ps => ElabVC.Conj $ Vector.concatV $ Vector.map (ps,
        fn p => case coercePredtoVCP p of 
          ElabVC.Simple s => Vector.new1 s
        | ElabVC.Conj sps => sps)
    | Forall _ => Error.bug "Impossible to coerce forall pred"

  fun coerceVCPtoPred (vcp : ElabVC.vc_pred) : pred =
    case vcp of
      ElabVC.Simple (ElabVC.True) => True
    | ElabVC.Simple (ElabVC.Base p) => Base p
    | ElabVC.Simple (ElabVC.Rel p) => Rel p
    | ElabVC.Conj sps => Conj $ Vector.map (sps, fn sp =>
        coerceVCPtoPred $ ElabVC.Simple sp)
    
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

  fun joinCons (el,vecvec) = case Vector.length vecvec of 
    0 => Vector.new1 $ Vector.new1 el
  | _ => Vector.map (vecvec, fn vec => Vector.concat 
    [Vector.new1 el, vec])

  (*
   * Vec[n] -> Vec[m] -> Vec [m*n]
   *)
  fun joinWith (vec1,vec2,f) = 
    case (Vector.length vec1, Vector.length vec2) of
      (0,_) => vec2 | (_,0) => vec1 
    | _ =>Vector.fromList $ vectorFoldrFoldr (vec1, vec2, [], 
      fn (el1,el2,acc) => (f (el1,el2))::acc)

  fun joinAllWith (vecvec,f) =
    Vector.fold (vecvec, Vector.new0 (), fn (vec,acc) =>
      joinWith (vec,acc,f))

  fun joinSols (sols1,sols2) =
    let
      fun disjointUnion (sol1,sol2) = 
        let
          val dom1 = Vector.toListMap (sol1,fst)
          val dom2 = Vector.toListMap (sol2,fst)
          val {areDisjoint, ...} = List.set {equals = RelVar.eq,
            layout = RelVar.layout}
          val _ = assert (areDisjoint (dom1,dom2), "Solution\
            \ domains not disjoint. Cannot join.")
        in
          Vector.concat [sol1,sol2]
        end
    in
       joinWith (sols1, sols2, disjointUnion)
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
    (output $ L.str "Verification Conditions:\n" ; 
      output $ layout vcs)

  fun isValidVC (re, ElabVC.T (tydbinds,anteP,conseqP)) =
    let
      val vc = T (tydbinds, coerceVCPtoPred anteP, 
        coerceVCPtoPred $ ElabVC.Simple conseqP)
     val _ = List.push (allvcs,vc)
    in
      true
    end

  fun allRVInsts (re,{rtyvs, cs, params}) : sol vector =
    let
      exception NotUnifiable
      (*
       * unifySorts can be merged with typeSynthIEApp from
       * elab. Unimpl.
       *)
      fun unifySorts (pts1,sps2) =
        let
          val ProjSort.T {paramsorts,sort} = PSS.specialize $ 
            PTS.specialize pts1
          val sps1 = if Vector.length paramsorts = 0 then sort
            else raise NotUnifiable
          val empty = Vector.new0 ()
          val (tydEqs,(rt1,rt2)) = (case (sps1,sps2) of
              (SPS.Base rt1, SPS.Base rt2) => (empty, (rt1,rt2))
            | (SPS.ColonArrow (tyd1,rt1), SPS.ColonArrow (tyd2,rt2)) 
              => (TyD.unify (tyd1,tyd2), (rt1,rt2))) handle _ =>
                raise NotUnifiable
          (*
           * tydEqs is valid iff its domain is subset of generalized
           * tyvars
           *)
          val PTS.T {tyvars, ...} = pts1
          val printTyDEqs = fn tydEqs => print $ Vector.toString
            (fn (v,tyd) => (Tyvar.toString v)^":="
              ^(TyD.toString tyd)) tydEqs
          val _ = if isValidInst (tydEqs,tyvars,tyVarEq) then ()
            else (raise NotUnifiable)
          val rtyc = RelTyC.new (rt2, RelType.instTyvars (tydEqs,rt1))
          val (rtyveqs,residue) = RelTyC.solvePartial $ 
            Vector.new1 rtyc
          val _ = if Vector.isEmpty residue then ()
            else raise NotUnifiable
        in
          rtyveqs
        end
      val revec = RE.toVector re
      val findCompatible = fn sps => Vector.keepAllMap (revec, 
        fn (rid,{ty,map,params}) => SOME (unifySorts (ty,sps), 
          RelLang.ieatomOfRel rid) handle NotUnifiable => NONE )
      fun instRelTyvars (eqs,ps) = List.map (ps,
        fn (rv,sps) => (rv,SPS.instRelTyvars (eqs,sps)))
      fun isSolComplete sol = Vector.forall (sol, fn (_,ridop) =>
        case ridop of SOME _ => true | _ => false)
      fun solOpToSol solOp = Vector.map (solOp, fn (rv,SOME rid) =>
        (rv,rid))
      fun doIt params = case params of
          [] => Vector.new0 ()
        | (rv,sps)::ps' => 
          let
            val compats = findCompatible sps
            val sls = case Vector.length compats of
                0 => Vector.new1 ((rv,NONE),ps')
              | _ => Vector.map (compats, fn (rtyveqs,rid) => 
                ((rv,SOME rid), instRelTyvars (rtyveqs, ps')))
            val solOps = Vector.concatV $ Vector.map (sls, 
              fn (eq,ps') => joinCons (eq, doIt ps'))
          in
            solOps
          end 
      val solOps = doIt $ Vector.toList params
      val completeSolOps = Vector.keepAll (solOps, isSolComplete)
      val sols = Vector.map (completeSolOps ,solOpToSol)
      val _ = Vector.foreachi (sols, fn (i,sol) =>
        let
          val istr = Int.toString i
          val _ = print ("Candidate rvinsts(" ^ istr ^ ") :\n")
          val _ = print $ Vector.toString (fn (rv,ieat) =>
            (RelVar.toString rv)^ " := "^
              (RelLang.ieatomToString ieat)) sol
          val _ = print "\n"
        in
          ()
        end)
    in
      sols
    end

  fun instRVars (eqs,pred) : pred =
    coercePTtoPred $ P.mapRExpr (coercePredToPT pred)
      (fn re => RelLang.instRelVars (eqs,re))

  fun firstOrderModels (re,pred : pred) : ElabVC.vc_pred vector =
    case pred of
      (*
       * Conj is a join point.
       *)
      Conj preds => joinAllWith (Vector.map (preds, fn pred =>
        firstOrderModels (re,pred)), vcPredConj)
    | Forall (x,p) => 
      let
        (*
         * Assertion: p must be simple pred)
         * If there are no first order models for this
         * second-order assertion, we simply drop it,
         * weakening the antecedent.
         *)
        val candidateSols = allRVInsts (re,x)
        val foPs = Vector.map (candidateSols, fn sol => 
          instRVars (sol,p))
        val foVCPs = Vector.map (foPs, coercePredtoVCP)
      in
        foVCPs
      end
    | _ => Vector.new1 $ coercePredtoVCP pred

  fun solveVC (re,T (tydbinds,anteP,conseqP)) : sol vector =
    let
      datatype t1 = T1 of tydbinds * ElabVC.vc_pred * ElabVC.vc_pred
      fun t1IsValid (T1 (tydbinds,anteVCP,conseqVCP)) =
        let
          val conseqPs = case conseqVCP of 
            ElabVC.Simple sp => Vector.new1 sp
          | ElabVC.Conj sps => sps
          val vcs = Vector.map (conseqPs, fn conseqP =>
            ElabVC.T (tydbinds, anteVCP, conseqP))
        in
          Vector.forall (vcs, fn vc => isValidVC (re,vc))
        end
      val emptysol = Vector.new0 ()
      val solConseqVCPPairs = case conseqP of 
        Forall (x,p) => 
          let
            val candidateSols = allRVInsts (re,x)
            val _ = case Vector.length candidateSols of
                0 => raise (UnsolvableVC "No candidate sols for\
                  \ 2nd order consequent.")
              | _ => ()
            val foConseqPs = Vector.map (candidateSols, 
              fn sol => instRVars (sol,p))
            val foConseqVCPs = Vector.map (foConseqPs,
              coercePredtoVCP)
          in
            Vector.zip (candidateSols,foConseqVCPs)
          end
      | p => Vector.new1 (emptysol, coercePredtoVCP p)
      val antePs = firstOrderModels (re,anteP)
      val solVCPairs = case conseqP of 
          True => Vector.map (solConseqVCPPairs, 
            fn (sol,_) => (sol, Vector.new0 ()))
        | _ => Vector.map (solConseqVCPPairs, fn (sol,conseqVCP) => 
          (sol, Vector.map (antePs, fn anteP => 
            T1 (tydbinds,anteP,conseqVCP))))
      (*
       * An instantiation (sol) is a solution for VC iff there 
       * exists a first order model of antecedent under which
       * the instantiated consequent is valid
       *)
      fun validateSol (sol,t1s) = case Vector.length t1s of
          0 => true
        | _ => Vector.exists (t1s, t1IsValid)
      (*
       * For first-order conseqP, validSols is a unary vector
       * of trivial (null) instantiation.
       *)
      val validSols : sol vector = Vector.map (Vector.keepAll 
        (solVCPairs, validateSol), fst)
      val _ = case Vector.length validSols of 
          0 => raise (UnsolvableVC "validSols is empty") | _ => ()
    in
      validSols
    end

  fun solveTypeCheck (ve, re, subSS, supSS) :
     sol vector =
    let
      val subSS = RefSS.rmUnusedRVars subSS
      val supSS = RefSS.rmUnusedRVars supSS
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
                vcOf (tydbinds, anteP, conseqP))
            val _ = List.push (allvcs,fullVC)
            val sol = solveVC (re,fullVC)
          in
            sol
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
        val _ = Control.saveToFile ({suffix = "evcs"}, Control.No, 
                 vcs, Control.Layouts layouts)
      in
        ()
      end
end

functor SpecVerify (S : SPEC_VERIFY_STRUCTS) : SPEC_VERIFY = 
struct
  open S
 
  structure SpecLang = VE.SpecLang
  structure VC = VerificationCondition (open SpecLang
                                        structure VE = VE
                                        (*structure ANormalCoreML = 
                                          ANormalCoreML*))
  open SpecLang
  open ANormalCoreML
  structure TyD = TypeDesc
  structure RefTy = RefinementType
  structure RefTyS = RefinementTypeScheme
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate

  type subst = Var.t*Var.t
  type substs = subst Vector.t
  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  val unifiable = TyD.unifiable
  fun toRefTyS refTy = RefTyS.generalize (Vector.new0(), refTy)
  val count = ref 4096
  fun getUniqueId symbase =
    let val id = symbase ^ (Int.toString (!count))
        val _ = count := !count + 1
    in
      Var.fromString id 
    end
  fun genVar () =  getUniqueId "sv_" 
  fun getUniqueMarker () = getUniqueId "_mark_"
  fun dummyRefTyS () = RefTyS.generalize (Vector.new0(),RefTy.fromTyD
    (TyD.makeTunknown()))
  fun varEq (v1,v2) = ((Var.toString v1) = (Var.toString v2))
  fun markVE ve = 
    let
      val marker = getUniqueMarker ()
    in
      (marker,VE.add ve (marker,dummyRefTyS()))
    end 

  (*
   * Produces a refTy' with base types taken from TyD and
   * refinements from refTy, given that user-provided base
   * types in refTy are unifiable with base types in tyd
   * Caution : tyvar unification is not uniform. 
   *)
  fun mergeTypes (tyd : TyD.t, refTy : RefTy.t) : RefTy.t =
    let
      open TyD
      open RefTy
      fun mergeErrorMsg (tyd,tyd') = "Cannot merge ML type " ^ (TyD.toString tyd)
        ^ " with type given in spec: " ^ (TyD.toString tyd')
      fun doMerge (tyd:TyD.t) (refTy:RefTy.t) = case (tyd,refTy) of
          (_,Base (bv,tyd',pred)) => (assert (unifiable (tyd,tyd'),
            mergeErrorMsg(tyd,tyd'));
            (Vector.new0 () ,Base (bv,tyd,pred)))
        | (Trecord tydr, Tuple reftyv) => 
            let
              val (vcs,reftyv') = (Vector.unzip o Vector.map2) 
                (Record.toVector tydr, reftyv, 
                  fn ((lbl,tyd'),Base (bv,tyd,pred)) => 
                    let
                      val _ = assert (unifiable (tyd',tyd), 
                        mergeErrorMsg(tyd',tyd))
                      val bv' = Var.fromString $ Field.toString lbl
                      val pred' = Predicate.applySubst (bv',bv) pred
                    in
                      ((bv',bv), Base (bv',tyd',pred'))
                    end)
            in
              (vcs, Tuple reftyv')
            end
        | (Tarrow (tyd1,tyd2), Arrow (refTy1,refTy2)) => 
            let
              val (substs,refTy'1) = doMerge tyd1 refTy1
              val (_,refTy'2) = doMerge tyd2 (RefTy.applySubsts substs refTy2)
            in
              (Vector.new0 (), Arrow (refTy'1,refTy'2))
            end
        | _ => raise Fail $ "Types Merge Error"
      val (_,refTy') = doMerge tyd refTy
    in
      refTy'
    end

  val elabPatVal = fn (ve,tyvars,patval,expty) =>
    let
      open Pat.Val
      fun elabPatVal (ve,patval,expty) : VE.t = case (patval,expty) of
        (Atom (Wild),_) => ve
      | (Atom (Const c),_) => Error.bug ("unimpl constant pats")
      | (Atom (Var v),_) => VE.add ve (v, RefTyS.generalize 
          (tyvars,expty))
      | (Tuple patatoms,RefTy.Tuple refTys) => Vector.fold2 
          (patatoms, refTys, ve, fn (patatom,refTy,ve) =>
            elabPatVal (ve,Atom patatom,refTy))
      | (Record patmrec, RefTy.Tuple refTys) => Vector.fold
          (Record.toVector patmrec, ve, fn ((lbl,patom),ve) =>
            let
              val indx = Vector.index (refTys, fn refTy =>
                case refTy of RefTy.Base (bv,_,_) => 
                  Field.toString lbl = Var.toString bv
                | _ => false)
              val refTy = case indx of SOME i => Vector.sub (refTys,i)
                | NONE => RefTy.fromTyD $ TyD.makeTunknown () 
                  (* Unimpl : Nested records *)
            in
              elabPatVal (ve, Atom patom, refTy)
            end)
      | _ => Error.bug ("Expression type is not row type.")
    in
      elabPatVal (ve,patval,expty)
    end

  (*
   * Invariant : bv and td of ty is unchanged.
   *)
  fun wellFormedType (marker : Var.t, markedVE : VE.t, ty : RefTy.t) 
    : RefTy.t =
    let
      (* we rely on the fact the VE is ordered *)
      val vevec = VE.toVector markedVE
      val indx = Vector.index (vevec, fn (v,_) => varEq (v,marker))
      val i = case indx of SOME i => i | _ => Error.bug "Marker absent"
      (*
       * tyvars are only generalized for function types and tuple types.
       * We do not need vars with such types as they are not referenced
       * from type refinements.
       *)
      val extyvec = Vector.map (Vector.prefix (vevec,i), fn (v,rtys) =>
        (v,RefTyS.specialize rtys))
      val (tyDB,pred1)= Vector.fold (extyvec,(TyDBinds.empty,Predicate.truee()),
        fn ((exvar,exty),(clos,pred')) =>
          case exty of
            RefTy.Base (bv,extd,pred) => (TyDBinds.add clos exvar extd,
              Predicate.conj (pred',Predicate.applySubst (exvar,bv) pred))
          | _ => (clos,pred'))
      (*
       *val _ = print "TyDBinds:\n"
       *val _ = Layout.print (TyDBinds.layout tyDB,print)
       *val _ = print "\n"
       *)
    in
      RefTy.mapBaseTy ty (fn (v,td,pred) => (v,td,
        Predicate.exists (tyDB,Predicate.conj(pred1,pred))))
    end

  (*
   * For functions with dependent types, bound variables within argument
   * types are refered from refinements of result types. At application
   * sites, the formal boundvars are substituted for actual program vars.
   * unifyArgs returns such substitutions.
   *)
  fun unifyArgs (argTy : RefTy.t,argExpVal : Exp.Val.t) : 
      ((Var.t*RefTy.t) vector * substs) =
    let
      open Exp.Val
    in
      case (argTy,argExpVal) of
        (RefTy.Base (argvar,td,p),Atom (Var (v,typv))) => 
          (Vector.new1 (v,RefTy.Base (argvar,td,p)),Vector.new1 (v,argvar))
      | (RefTy.Base (argvar,td,p),Atom (Const c)) => Error.bug $ 
          "Unimpl const args"
      | (RefTy.Tuple argTys,Tuple atomvec) => 
          let
            val (reftyss,substss) = (Vector.unzip o Vector.map2)
              (argTys,atomvec, fn (argTy,atom) => unifyArgs (argTy, Atom atom))
          in
            (Vector.concatV reftyss, Vector.concatV substss)
          end
      | (RefTy.Tuple argTys,Record atomrec) => 
          let
            val (reftyss,substss)= (Vector.unzip o Vector.map)
            (argTys, fn (argTy) =>
              let
                val bv = case argTy of RefTy.Base (bv,_,_) => bv
                  | _ => Error.bug "Unimpl Nested records"
                val lblatomvec = Record.toVector atomrec
                val indx = Vector.index (lblatomvec, fn (lbl,_) =>
                  Field.toString lbl = Var.toString bv)
                val (_,atom) = case indx of SOME i => Vector.sub (lblatomvec,i)
                  | NONE => Error.bug ("Field " ^ (Var.toString bv) ^ 
                      " could not be found.")
              in
                unifyArgs (argTy,Atom atom)
              end)
          in
            (Vector.concatV reftyss, Vector.concatV substss)
          end
      | (RefTy.Tuple _, Atom _) => Error.bug "Unimplemented Tuple-Atom"
      | (RefTy.Arrow _, _) => (Vector.new0 (), Vector.new0 ())
      | _ => raise Fail $ "Invalid argTy-argExpVal pair encountered"
    end

    fun unifyWithDisj (refTy1 : RefTy.t,refTy2 : RefTy.t) : RefTy.t =
      let
        open RefTy
      in
        case (refTy1,refTy2) of
          (Base (bv1,td1,pred1),Base (bv2,td2,pred2)) =>
            let
              val _ = assert (TyD.sameType (td1,td2), "Typedescs from \
                \ two branches of case did not match. Two types are:\n \
                \ 1. "^(TyD.toString td1) ^"\n" ^ "\
                \ 2. "^(TyD.toString td2) ^ "\n")
              val pred1' = Predicate.applySubst (bv2,bv1) pred1
            in
              Base (bv2,td2,Predicate.disj(pred1,pred2))
            end
        | (Tuple t1,Tuple t2) => (Tuple o Vector.map2) (t1,t2, 
            fn (r1,r2) => case (r1,r2) of
              (Base (bv1,td1,pred1),Base (bv2,td2,pred2)) =>
                let
                  val _ = assert (varEq (bv1,bv2), "Labels of tuples from \
                    \ two branches of case did not match")
                in
                 unifyWithDisj (r1,r2)
                end
            | _ => unifyWithDisj (r1,r2))
        | (Arrow _,Arrow _) => Error.bug "Unimpl : Case returning arrow"
        | _ => Error.bug "Case rules types not unifiable"
      end 

  fun typeSynthValExp (ve:VE.t, valexp : Exp.Val.t) : RefTy.t = 
    let
      open Exp
    in
      case valexp of
        Val.Atom (Val.Const c) => RefTy.fromTyD (TyD.makeTunknown ())
      | Val.Atom (Val.Var (v,typv)) =>  
        let
          val tydvec = Vector.map (typv,Type.toMyType)
          val vtys = VE.find ve v handle (VE.VarNotFound _) => Error.bug
            ((Var.toString v) ^ " not found in the current environment\n")
          val vty = RefTyS.instantiate (vtys,tydvec)  
          (*
           * Keep track of variable equality.
           *)
          val qualifiedvty = case vty of RefTy.Base (bv,td,pred) => 
              RefTy.Base (bv,td,Predicate.conjP(pred,BP.varEq(bv,v)))
            | _ => vty (* Unimpl : refinements for fns *)
        in
          qualifiedvty
        end
      | Val.Tuple atomvec => RefTy.Tuple $ Vector.mapi (atomvec, fn (i,atm) => 
          let
            val atmTy = typeSynthValExp (ve,Val.Atom atm)
            val newbv = Var.fromString $ Int.toString (i+1) (* tuple BVs *)
          in
            RefTy.alphaRename atmTy newbv
          end)
      | Val.Record atomrec => RefTy.Tuple $ Vector.map (Record.toVector atomrec, 
          fn (lbl,atm) => 
            let
              val atmTy = typeSynthValExp (ve,Val.Atom atm)
              val newbv = Var.fromString $ Field.toString lbl (* Record BVs *)
            in
              RefTy.alphaRename atmTy newbv
            end)
    end

  fun typeSynthExp (ve : VE.t, exp : Exp.t) : VC.t vector * RefTy.t =
    let
      open Exp
      val expTy = ty exp
      val trivialAns = fn _ => (Vector.new0(), RefTy.fromTyD $ 
        Type.toMyType expTy)
    in
      case node exp of
        App (f,valexp) => 
          let
            val fty  = typeSynthValExp (ve,Val.Atom f)
            val (fargty,fresty)  =case fty of RefTy.Arrow x => x
              | _ => Error.bug ("Type of " ^ (Layout.toString $ 
                Exp.Val.layout $ Val.Atom f) ^ " not an arrow")
            val argTy = typeSynthValExp (ve,valexp)
            (*
             *  Γ ⊢ argTy <: fargty
             *)
            val vcs = VC.fromTypeCheck (ve,argTy,fargty)
            (*
             * Then, determine type of this expression by substituion of
             * actuals for formals.
             *)
            val (_,substs) = unifyArgs (fargty,valexp)
            val resTy = RefTy.applySubsts substs fresty
          in
            (vcs,resTy)
          end
      | Case {test:Exp.Val.t,rules,...} => 
          let
            val (wfTypes,vcs) = Vector.mapAndFold (rules, Vector.new0(), 
              fn ({pat,exp,...},vcs) =>
              let
                val valbind = Dec.PatBind (pat,test)
                val (marker,markedVE) = markVE ve
                (* 
                 * tyvars used, if any, for type instantiations inside
                 * test are bound at any of the enclosing valbinds. Therefore,
                 * passing empty for tyvars is sound.
                 *)
                val (vcs1,extendedVE) = doItValBind (markedVE,
                  Vector.new0(),valbind)
                val (vcs2,ty) = typeSynthExp (extendedVE,exp)
                val wftype = wellFormedType (marker,extendedVE,ty)
              in
                (wftype, Vector.concat [vcs, vcs1, vcs2])
              end)
            val (wfTypes,wfType) = Vector.splitLast wfTypes
            val unifiedType = Vector.fold (wfTypes, wfType, unifyWithDisj)
          in
            (*
             * 1. alphaRename boundvars forall wfTypes to a single var
             * 2. assert that tyd is same for all
             * 3. fold all alpha-renamed preds of wftypes with Disj
             *)
            (vcs,unifiedType)
          end
      | EnterLeave _ => trivialAns ()
      | Handle _ => trivialAns ()
      | Lambda l => typeSynthLambda (ve,l)
      | Let (decs,subExp) => 
        let
          val (marker,markedVE) = markVE ve
          val (vcs1,extendedVE) = doItDecs (markedVE,decs)
          val (vcs2,subExpTy) = typeSynthExp (extendedVE,subExp)
          (*
           *val _ = print "Let expression\n"
           *val _ = Layout.print (VE.layout extendedVE,print)
           *val _ = print "\n"
           *)
        in
          (Vector.concat [vcs1, vcs2], 
            wellFormedType (marker,extendedVE,subExpTy))
        end
      | PrimApp {args, prim, targs} => trivialAns ()
      | Raise _ => trivialAns ()
      | Seq tv => typeSynthExp (ve,Vector.last tv)
      | Value v => (Vector.new0(),typeSynthValExp (ve,v))
    end

  and typeSynthLambda (ve : VE.t,lam : Lambda.t) : (VC.t vector * RefTy.t) =
    let
      val {arg,argType,body} = Lambda.dest lam
      val argRefTy = RefTy.fromTyD (Type.toMyType argType)
      val extendedVE = VE.add ve (arg,toRefTyS argRefTy)
      (*
       * Γ[arg↦argTy] ⊢ body => bodyTy
       *)
      val (bodyvcs,bodyRefTy) = typeSynthExp (extendedVE,body)
    in
      (bodyvcs,RefTy.Arrow (argRefTy,bodyRefTy))
    end

  and typeCheckLambda (ve : VE.t,lam : Lambda.t, ty : RefTy.t) : VC.t vector =
    let
      val (argTy,resTy) = case ty of RefTy.Arrow v => v
        | _ => Error.bug "Function with non-arrow type"
      val {arg,body,...} = Lambda.dest lam
      val extendedVE = VE.add ve (arg, toRefTyS argTy)
      (*
       * Γ[arg↦argTy] ⊢ body => bodyTy
       *)
      val (bodyvcs,bodyTy) = typeSynthExp (extendedVE,body)
      (*
       * Γ[arg↦argTy] ⊢ bodyTy <: resTy
       *)
      val newvcs = VC.fromTypeCheck (extendedVE,bodyTy,resTy)
    in
      Vector.concat [bodyvcs,newvcs]
    end

  and doItValBind (ve,tyvars,valbind) : (VC.t vector * VE.t) = 
    case valbind of
      Dec.ExpBind (patval,exp) =>
        let
          val (vcs,expty) = typeSynthExp (ve,exp)
        in
          (vcs,elabPatVal (ve,tyvars,patval,expty))
        end
    | Dec.PatBind (pat,expval) =>
        let
          val patnode = Pat.node pat
          (*  unimpl : Exp.Val.ty *)
          val (vcs,expty) = typeSynthExp (ve,Exp.make (Exp.Value expval,
            Type.var $ Tyvar.newNoname {equality=false}))
          (*
           * This function is an unfortunate consequence of having separate
           * Pat.Val and Exp.Val. Unimpl : Coalesce.
           *)
          fun patValToExpVal (patval:Pat.Val.t) : Exp.Val.t = 
            let
              fun patAtomToExpAtom patatom = case patatom of
                  Pat.Val.Const c => Exp.Val.Const c
                | Pat.Val.Var v => Exp.Val.Var (v,Vector.new0 ())
            in
              case patval of
              Pat.Val.Atom (Pat.Val.Wild) => Error.bug "Impossible case wild"
            | Pat.Val.Atom atm => Exp.Val.Atom (patAtomToExpAtom atm)
            | Pat.Val.Tuple atomvec => Exp.Val.Tuple $ Vector.map (atomvec,
                patAtomToExpAtom)
            | Pat.Val.Record atomrec => 
              let
                val lblexpatmvec = Vector.map (Record.toVector atomrec,
                  fn (lbl,atom) => (lbl,patAtomToExpAtom atom))
              in
                Exp.Val.Record $ Record.fromVector lblexpatmvec
              end
            end 
        in
          case patnode of 
              Pat.Value patval => (vcs, elabPatVal
                (ve,tyvars,patval,expty))
            | Pat.Con {arg : Pat.Val.t option,con,targs} => 
              let
                val rhsvar = case expval of 
                    Exp.Val.Atom (Exp.Val.Var (v,_)) => v
                  | _ => Error.bug "A var is expected on rhs for conpat bind."
                val tydargs = Vector.map (targs,Type.toMyType)
                val convid = Var.fromString $ Con.toString con
                val conTyS = VE.find ve convid handle (VE.VarNotFound _) =>
                  Error.bug ("Constructor "^(Var.toString convid) ^ 
                    " not found in current env")
                val conTy  = RefTyS.instantiate (conTyS,tydargs)
                (*
                 * extend ve with new bindings for matched arguments.
                 *)
                val (ve',resTy) = case (conTy,arg) of 
                    (RefTy.Base _,NONE) => (ve,conTy)
                  | (RefTy.Base _, SOME _) => Error.bug "Arguments to nullary \
                      \ cons"
                  | (RefTy.Arrow _, SOME (Pat.Val.Atom (Pat.Val.Wild))) =>
                      (ve,conTy)
                  | (RefTy.Arrow (conArgTy,conResTy), SOME argPatVal) => 
                      let
                        val argExpVal = patValToExpVal argPatVal
                        val (argTyMap,substs) = unifyArgs (conArgTy,argExpVal)
                        val ve' = Vector.fold (argTyMap, ve, fn ((arg,refTy),ve) =>
                          VE.add ve $ (arg,RefTyS.generalize (tyvars,refTy)))
                      in
                        (ve', RefTy.applySubsts substs conResTy)
                      end
                  | _ => Error.bug "Impossible cons args"
                (*
                 * Extend ve' with a new dummy var to keep track of
                 * relationship between matched arguments and rhsvar.
                 *)
                val RefTy.Base (bv,td,p) = RefTy.alphaRename resTy rhsvar
                val _ = assert (Var.toString bv = Var.toString rhsvar, 
                  "RefTy alpha rename incorrect")
                val newTyS = RefTyS.generalize (tyvars, RefTy.Base (genVar(),
                  td, p))
              in
                (vcs, VE.add ve'(genVar(), newTyS))
              end
        end

  and doItDecs (ve : VE.t, decs : Dec.t vector) : (VC.t vector * VE.t) =
    let
      fun elabRecDecs (ve : VE.t) (tyvars : Tyvar.t vector)  decs = 
        Vector.fold (decs,ve, fn ({lambda : Lambda.t, var : Var.t},ve) =>
          let
            val {arg,argType,body} = Lambda.dest lambda
            val argTyD = Type.toMyType argType
            val bodyTyD = Type.toMyType $ Exp.ty body
            val funTyD = TyD.makeTarrow (argTyD,bodyTyD)
            val funRefTy = mergeTypes (funTyD, RefTyS.specialize
              $ VE.find ve var) handle (VE.VarNotFound _) => RefTy.fromTyD funTyD
            val funspec = RefTyS.generalize (tyvars,funRefTy)
          in
            VE.add ve (var,funspec)
          end)

      fun doItDec (ve : VE.t, dec : Dec.t) : (VC.t vector * VE.t) = case dec of
          Dec.Fun {decs,tyvars} => 
            let
              val extendedVE = elabRecDecs ve (tyvars()) decs
              val vcs = (Vector.concatV o Vector.map) (decs,
                fn ({lambda,var}) => 
                  let
                    val fty = RefTyS.specialize $ VE.find ve var
                      handle (VE.VarNotFound _) => Error.bug "ImpossibleCase!"
                    (*
                     * For recursive function lambdas are checked against 
                     * user-provided type or trivial type.
                     *)
                    val vcs = typeCheckLambda (extendedVE, lambda, fty)
                  in
                    vcs
                  end)
            in
              (vcs,extendedVE)
            end
        | Dec.Val {rvbs,tyvars,vbs} => 
            let
              val (rvcs,rvbsVE) = doItDec (ve,Dec.Fun {decs=rvbs,tyvars=tyvars})
              val (vcss,vbsVE) = Vector.mapAndFold (vbs,rvbsVE,fn ({valbind,...},ve) =>
                doItValBind (ve,tyvars(),valbind))
            in
              (Vector.concat [rvcs,Vector.concatV vcss] , vbsVE)
            end
        | _ => (Vector.new0 (), ve)

      val (vcsvec,extendedVE) = Vector.mapAndFold (decs,ve, 
        fn (dec,ve) => doItDec (ve,dec))

    in
      (Vector.concatV vcsvec, extendedVE)
    end

  fun doIt (ve, Program.T{decs}) = #1 $ doItDecs (ve,decs)

end

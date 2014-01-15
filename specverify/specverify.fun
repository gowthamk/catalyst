functor SpecVerify (S : SPEC_VERIFY_STRUCTS) : SPEC_VERIFY = 
struct
  open S
 
  structure SpecLang = VE.SpecLang
  structure VC = VerificationCondition (open SpecLang
                                        structure VE = VE
                                        structure RE = RE)
  open SpecLang
  open ANormalCoreML
  structure TyD = TypeDesc
  structure PSS = ProjSortScheme
  structure PTS = ProjTypeScheme
  structure RefTy = RefinementType
  structure RefTyS = RefinementTypeScheme
  structure RefSS = RefinementSortScheme
  structure P = Predicate
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure L = Layout

  type subst = Var.t*Var.t
  type substs = subst Vector.t
  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  val ignore = fn x => ()
  val unifiable = TyD.unifiable
  val toRefSS = RefSS.fromRefTy
  fun toRefTyS refss = RefTyS.generalize (Vector.new0(), refss)
  val count = ref 4096
  fun getUniqueId symbase =
    let val id = symbase ^ (Int.toString (!count))
        val _ = count := !count + 1
    in
      Var.fromString id 
    end
  fun genVar () =  getUniqueId "sv_" 
  fun getUniqueMarker () = getUniqueId "_mark_"
  fun dummyRefTyS () = RefTyS.generalize (Vector.new0(),
    RefSS.fromRefTy $ RefTy.fromTyD (TyD.makeTunknown()))
  val newLongVar = fn (var,fld) => Var.fromString $
    (Var.toString var)^"."^(Var.toString fld)
  fun varEq (v1,v2) = ((Var.toString v1) = (Var.toString v2))
  val varToExpVal = fn (var,tyvec) => 
    Exp.Val.Atom (Exp.Val.Var (var,tyvec))
  fun markVE ve = 
    let
      val marker = getUniqueMarker ()
    in
      (marker,VE.add ve (marker,dummyRefTyS()))
    end 
  (*
   * Add uninterpreted relation to RE
   *)
  fun addURelToRE re (rid,sps) =
    let
      val empty = fn _ => Vector.new0 ()
      val ps = ProjSort.simple sps
      val pss = ProjSortScheme.generalizeWith (empty (),
        empty (), ps)
      val pts = PTS.generalizeWith (empty (), pss)
      val desc = {ty = pts, params = empty (), map = empty ()}
    in
      RE.add re (rid,desc)
    end

  (*
   * Instantiate relvars in refty multiple times.
   *)
  fun multiInstRelParams (sols, refty) =
    let
      val doInst = fn _ => RefTy.unifyWithConj $ Vector.map 
        (sols, fn sol => if Vector.isEmpty sol then refty 
          else RefTy.instRelVars (sol,refty))
    in
      if Vector.isEmpty sols then refty else doInst ()
    end

  fun rmUnusedRVars refSS = 
    let
      val RefSS.T {reltyvars, constraints, paramrefty} = refSS
      val typedParams = RefSS.typedParams paramrefty
      val refTy = RefSS.toRefTy refSS
      val {add, contains, ...} = List.set {equals = RelVar.eq, 
        layout = RelVar.layout }
      val used = RefTy.foldRExpr refTy [] 
        (fn (e,acc) => RelLang.foldRVar e acc
          (fn (rv,acc2) => add (acc2,rv)))
      val params' = Vector.keepAll (typedParams, 
        fn (rv,_) => contains (used,rv))
      val prt' = RefSS.paramRefTy (params',refTy)
    in
      RefSS.generalize (constraints, prt')
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
      fun isTupleTyD (row : 'a Record.t) = 
        (*
         * SML tuples are records with consecutive numeric
         * fields starting with 1.
         *)
        let 
          val lbltydv = Record.toVector row 
        in
          case Vector.length lbltydv of 0 => true 
          | _ => Vector.foralli (lbltydv, fn (i,(lbl,_)) => 
              Field.toString lbl = Int.toString (i+1))
        end
      fun mergeErrorMsg (tyd,tyd') = "Cannot merge ML type " ^ (TyD.toString tyd)
        ^ " with type given in spec: " ^ (TyD.toString tyd')
      fun doMerge (tyd:TyD.t) (argBind as (argv: Var.t, refTy:RefTy.t)) = 
        case (tyd,refTy) of
          (_,Base (bv,tyd',pred)) => 
            let
              val _ = assert (unifiable (tyd,tyd'),
                mergeErrorMsg(tyd,tyd'))
              val newArgBind = (argv, Base (bv,tyd,pred))
            in
              (Vector.new0 (), newArgBind)
            end 
        | (Trecord tydr, Tuple argBinds') => (case isTupleTyD tydr of
            true =>
              let
                val (substss,newArgBinds') = (Vector.unzip o Vector.map2) 
                  (Record.toVector tydr, argBinds', 
                  fn ((lbl,tyd'),argBind' as (argv',refty')) => 
                    let
                      (*
                       * Argvar for tuple fields used in spec should be 
                       * substituted with field label.
                       *)
                      val newargv' = Var.fromString $ Field.toString lbl
                      val (substs,newArgBind) = doMerge tyd' (newargv',refty')
                      val substs = Vector.concat [Vector.new1 (newargv',argv'), 
                        substs]
                    in
                      (substs,newArgBind)
                    end)
                val substs = Vector.map (Vector.concatV substss,
                  fn (n,ol) =>  (newLongVar (argv,n), ol))
                val newArgBind = (argv, Tuple newArgBinds')
              in
                (substs, newArgBind)
              end
          | false => raise (Fail "Unimpl"))
        | (Tarrow (tyd1,tyd2), Arrow (argBind,resTy)) => 
            let
              val (substs,argBind') = doMerge tyd1 argBind
              val dummyArgVar = argv
              val (_,(_,resTy')) = doMerge tyd2 (dummyArgVar, 
                RefTy.applySubsts substs resTy)
              val newArgBind = (argv, Arrow (argBind',resTy'))
            in
              (Vector.new0 (), newArgBind)
            end
        | _ => raise Fail $ "Types Merge Error"
      val (_,(_,refTy')) = doMerge tyd (genVar (), refTy)
    in
      refTy'
    end

  fun mergeTypeToSS re (tyd, ss) =
    let
      val RefSS.T {paramrefty, ...} = ss
      val refTy = mergeTypes (tyd, RefSS.toRefTy ss)
      val _ = print "RefTy is\n"
      val _ = Control.message (Control.Top, fn _ =>
        RefTy.layout refTy)
      val ss = Elab.sortSchemeOfRefTy re refTy
    in
      ss
    end

  val elabPatVal = fn (ve,re,tyvars,patval,expss) =>
    let
      open Pat.Val
      val expty = RefSS.toRefTy expss
      val err = fn _ => String.concat 
        ["Expression type is not row type.\n",
          "Pattern is ",L.toString $ layout patval,"\n",
          "Type of expression is ",L.toString $ RefTy.layout expty,
            "\n" ]
      val len = Vector.length
      fun elabPatVal (ve,patval,expss) : VE.t = 
        case (patval,RefSS.toRefTy expss) of
        (Atom (Wild),_) => ve
      | (Atom (Const c),_) => Error.bug ("unimpl constant pats")
      | (Atom (Var v),_) => VE.add ve (v, RefTyS.generalize 
          (tyvars,expss))
        (*
         * For tuple and record patterns, reltyvars and relvars
         * for patvars are same as the matched expression.
         * Unimpl : remove un-referred.
         *)
      | (Tuple patatoms,_) => 
        (case (len patatoms,RefSS.toRefTy expss) of 
          (* Unit tuples are atoms *)
          (1,_) => elabPatVal (ve,Atom (Vector.sub (patatoms,0)),
            expss)
        | (_, RefTy.Tuple refTyBinds) => Vector.fold2 (patatoms, 
            refTyBinds, ve, fn (patatom,(_,refTy),ve) =>
              elabPatVal (ve, Atom patatom, RefSS.substRefTy 
                (expss,refTy)))
        | _ => Error.bug $ err ())
      | (Record patmrec, RefTy.Tuple refTyBinds) => Vector.fold
          (Record.toVector patmrec, ve, fn ((lbl,patom),ve) =>
            let
              val indx = Vector.index (refTyBinds, fn (fldvar,_) =>
                  Field.toString lbl = Var.toString fldvar)
              val refTyBind as (_,refTy) = case indx of 
                  SOME i => Vector.sub (refTyBinds,i)
                | NONE => Error.bug $ "Record field not \
                  \found\n"^(err ())
            in
              elabPatVal (ve, Atom patom, RefSS.substRefTy 
                (expss,refTy))
            end)
      | _ => Error.bug $ err()
    in
      elabPatVal (ve,patval,expss)
    end

  (*
   * Decomposes single tuple bind of form v ↦ {x0:T0,x1:T1} to
   * multiple binds : [v.x0 ↦ T0, v.x1 ↦ T1]
   *)
  fun decomposeTupleBind (tvar : Var.t, tty as RefTy.Tuple refTyBinds) 
    : (Var.t*RefTy.t) vector =
    let
      val bindss = Vector.map (refTyBinds, fn (refTyBind as (_,refTy))
        => case refTy of RefTy.Tuple _ => decomposeTupleBind refTyBind
        | _ => Vector.new1 refTyBind)
      val binds = Vector.map (Vector.concatV bindss, fn (v,ty) =>
        (newLongVar (tvar,v), ty))
    in
      binds
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
      val i = case indx of SOME i => i 
        | _ => Error.bug "Marker absent"
      (*
       * tyvars are only generalized for function types and tuple 
       * types. We do not need vars with such types as they are not 
       * referenced from type refinements.
       *)
      val exssvec = Vector.map (Vector.prefix (vevec,i), 
        fn (v,rtys) => (v,RefTyS.specialize rtys))
      val flattened = Vector.concatV $ Vector.map (exssvec, 
        fn (v,ss) => 
          let
            val ty = RefSS.toRefTy ss
            val tyatoms = case ty of 
              RefTy.Tuple _ => decomposeTupleBind (v,ty)
            | _ => Vector.new1 (v,ty)
          in
            Vector.map (tyatoms, fn (v,refTy) => (v,
              RefSS.substRefTy (ss,refTy)))
          end)
      val (tyDB,pred1)= Vector.fold (flattened,(TyDBinds.empty,
        Predicate.truee()), fn ((exvar,exss),(clos,pred')) =>
          let
            val (RefSS.T {reltyvars = rs,constraints = cs 
              ,paramrefty}) = exss
            val params = RefSS.typedParams paramrefty
            val exty = RefSS.toRefTy exss
            val closeAndConj = fn (bv,pred) => Predicate.conj (pred',
              Predicate.forall (rs, cs, params, 
                Predicate.applySubst (exvar,bv) pred))
          in
            case exty of
              RefTy.Base (bv,extd,pred) => (TyDBinds.add clos 
                exvar extd, closeAndConj (bv,pred))
            | RefTy.Tuple _ => Error.bug "Tuple flattening incorrect\n"
            | _ => (clos,pred')
          end)
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
   * sites, actual program vars are substituted for formal boundvars.
   * unifyArgs returns such substitutions.
   * unifyArgs also returns new type bindings for actual vars. This is for
   * the sake of constructor pattern matches, where the returned type binds
   * contain type bindings for matched pattern vars.
   *)
  fun unifyArgs (argBind as (argv : Var.t, argTy : RefTy.t) ,
      argExpVal : Exp.Val.t) : ((Var.t*RefTy.t) vector * substs) =
    let
      open Exp.Val
    in
      case (argTy,argExpVal) of
        (RefTy.Base _, Atom (Var (v,typv))) => 
          (Vector.new1 (v,argTy), Vector.new1 (v,argv))
      | (RefTy.Base _,Atom (Const c)) => Error.bug $ 
          "Unimpl const args"
      | (RefTy.Tuple argBinds',Tuple atomvec) => 
          (*
           * Unifies v:{1:T0,2:T1} with (v0,v1)
           * Returns binds = [v0 ↦ T0, v1 ↦ T1],
           *        substs = [v0/v.1, v1/v.2]
           *)
          let
            val (reftyss,substss) = (Vector.unzip o Vector.map2)
              (argBinds',atomvec, fn (argBind',atom) => 
                let
                  val (binds,substs') = unifyArgs (argBind', Atom atom)
                  val substs = Vector.map (substs', 
                    fn (n,o') => (n, newLongVar (argv,o')))
                in
                  (binds,substs)
                end)
          in
            (Vector.concatV reftyss, Vector.concatV substss)
          end
      | (RefTy.Tuple argBinds', Record atomrec) => 
          let
            val (reftyss,substss)= (Vector.unzip o Vector.map)
            (argBinds', fn (argBind') =>
              let
                val (argv',_) = argBind'
                val argvStr' = Var.toString argv'
                val lblatomvec = Record.toVector atomrec
                val indx = Vector.index (lblatomvec, fn (lbl,_) =>
                  Field.toString lbl = argvStr')
                val (_,atom) = case indx of 
                    SOME i => Vector.sub (lblatomvec,i)
                  | NONE => Error.bug ("Field " ^ (argvStr') ^ 
                      " could not be found.")
                val (binds,substs') = unifyArgs (argBind',Atom atom)
                  val substs = Vector.map (substs', 
                    fn (n,o') => (n, newLongVar (argv,o')))
              in
                (binds,substs)
              end)
          in
            (Vector.concatV reftyss, Vector.concatV substss)
          end
      | (RefTy.Tuple argBinds', Atom (Var (v,_))) => 
          let
            (* Unifying v0:{x0:T0,x1:T1} with v1 would return
             * v1 ↦ {x0:T0,x1:T1} as the only new bind. However,
             * all references v0 elements should now refer to v1
             * elements. Therefore, substs = [v1.x0/v0.x0, v1.x1/v0.x1]
             *)
            val binds = Vector.new1 (v,argTy)
            val substs = (Vector.concatV o Vector.map)
            (argBinds', fn (argBind') =>
              let
                val (argv',_) = argBind'
                val newVar = newLongVar (v,argv')
                val (_,substs') = unifyArgs (argBind',
                  Atom (Var (newVar, Vector.new0 ())))
                val substs = Vector.map (substs', 
                  fn (n,o') => (n, newLongVar (argv,o')))
              in
                substs
              end)
          in
            (binds,substs)
          end
      | (RefTy.Arrow _, Atom (Var (v,_))) => (Vector.new1 (v,argTy), 
          Vector.new1 (v,argv))
      | _ => raise Fail $ "Invalid argTy-argExpVal pair encountered"
    end

  fun typeSynthValExp (ve:VE.t, re:RE.t, valexp : Exp.Val.t) 
      : RefSS.t = 
    let
      open Exp
    in
      case valexp of
        Val.Atom (Val.Const c) => RefSS.fromRefTy $ RefTy.fromTyD 
          (TyD.makeTunknown ())
      | Val.Atom (Val.Var (v,typv)) =>  
        let
          val tydvec = Vector.map (typv,Type.toMyType)
          val vtys = VE.find ve v handle (VE.VarNotFound _) => Error.bug
            ((Var.toString v) ^ " not found in the current environment\n")
          val vss = RefTyS.instantiate (vtys,tydvec)  
          val vty = RefSS.toRefTy vss
          (*
           * Keep track of variable equality.
           * We currently cannot keep track of equality if rhs
           * is a nullary constructor or a function. Unimpl.
           *)
          val qualifiedvty = case (Vector.length tydvec, vty) of 
              (0,RefTy.Base (bv,td,pred)) => RefTy.Base 
                (bv,td,Predicate.conjP(pred,BP.varEq(bv,v)))
            | (_,RefTy.Tuple refTyBinds) => RefTy.Tuple $ Vector.map
                (refTyBinds, fn (fldvar,refty) => 
                  let
                    val newvar = newLongVar (v,fldvar)
                    val extendedVE = VE.add ve (newvar,toRefTyS $ toRefSS refty)
                    val newvarexp = varToExpVal (newvar, Vector.new0 ())
                    val refty' = RefSS.toRefTy $ typeSynthValExp 
                      (extendedVE, re, newvarexp)
                  in
                    (fldvar, refty')
                  end)
            | _ => vty (* Unimpl : refinements for fns. Cannot keep
                   track of equality for functions now.*)
        in
          RefSS.substRefTy (vss,qualifiedvty)
        end
      | Val.Tuple atomvec => RefSS.coalesce (Vector.map (atomvec,
          fn atm => RefSS.alphaRename $ typeSynthValExp 
            (ve, re, Val.Atom atm)))
          (fn refTyv => (RefTy.Tuple o Vector.mapi) (refTyv, 
            fn (i,refTy) => 
              let
                (* tuple BVs *)
                val fldvar = Var.fromString $ Int.toString (i+1) 
              in
                (fldvar, refTy)
              end))
      | Val.Record atomrec => RefSS.coalesce (Vector.map
          (Record.toVector atomrec, fn (_,atm) => 
            RefSS.alphaRename $ typeSynthValExp 
              (ve, re, Val.Atom atm)))
          (fn refTyv => (RefTy.Tuple o Vector.map2) (
            Record.toVector atomrec, refTyv, fn ((lbl,_),refTy) => 
              let
                (* Record BVs *)
                val fldvar = Var.fromString $ Field.toString lbl 
              in
                (fldvar, refTy)
              end))
    end

  fun typeSynthExp (ve : VE.t, re : RE.t, exp : Exp.t) 
      : RefSS.t =
    let
      open Exp
      val expTy = ty exp
      val trivialAns = fn _ => RefSS.fromRefTy $ RefTy.fromTyD $ 
        Type.toMyType expTy
    in
      case node exp of
        App (f,valexp) => 
          let
            val fss = typeSynthValExp 
              (ve, re, Val.Atom f)
            val _ = print "Sortscheme of fn:\n"
            val _ = Control.message (Control.Top, fn _ =>
              RefSS.layout fss)
            val fss = RefSS.alphaRename fss
            val _ = print "Sortscheme of fn (after alpha):\n"
            val _ = Control.message (Control.Top, fn _ =>
              RefSS.layout fss)
            val fty = RefSS.toRefTy fss
            val (fargBind as (farg,fargty),fresty)  = case fty of 
                RefTy.Arrow x => x
              | _ => Error.bug ("Type of " ^ (Layout.toString $ 
                Exp.Val.layout $ Val.Atom f) ^ " not an arrow")
            val fargSS = RefSS.substRefTy (fss,fargty)
            val argSS = typeSynthValExp (ve,re,valexp)
            (*
             * Find instantiations for generalized rel params that
             * satisfy the judgement:
             *  Γ ⊢ argSS <: fargSS
             *)
            val sols = VC.solveTypeCheck (ve, re, argSS, fargSS)
            val (binds,substs) = unifyArgs (fargBind, valexp)
            (*
             * Then, determine type of this expression by substitution 
             * of actuals for formals.
             *)
            val resTy = RefTy.applySubsts substs $ 
              multiInstRelParams (sols, fresty)
            val _ = print "Result type of app:\n"
            val _ = Control.message (Control.Top, fn _ =>
              RefTy.layout resTy)
            val templateResSS = RefSS.substRefTy (fss,resTy)
            val resSS = rmUnusedRVars templateResSS
            val _ = print "Result sortscheme of app:\n"
            val _ = Control.message (Control.Top, fn _ =>
              RefSS.layout resSS)
          in
            resSS
          end
      | Case _ => Error.bug "Unimpl: synthesizing type for case"
      | EnterLeave _ => trivialAns ()
      | Handle _ => trivialAns ()
      | Lambda l => typeSynthLambda (ve,re,l)
      | Let (decs,subExp) => 
        let
          val (marker,markedVE) = markVE ve
          val extendedVE = doItDecs (markedVE,re,decs)
          val subExpSS = typeSynthExp (extendedVE,re,subExp)
          val subExpTy = RefSS.toRefTy subExpSS
          val wfTy = wellFormedType (marker,extendedVE,subExpTy)
          val wfSS = RefSS.substRefTy (subExpSS,wfTy)
        in
          wfSS
        end
      | PrimApp {args, prim, targs} => trivialAns ()
      | Raise _ => trivialAns ()
      | Seq tv => typeSynthExp (ve,re,Vector.last tv)
      | Value v => typeSynthValExp (ve,re,v)
    end

  and typeSynthLambda (ve : VE.t, re : RE.t, lam : Lambda.t) 
      : RefSS.t =
    let
      val {arg,argType,body} = Lambda.dest lam
      val argRefTy = RefTy.fromTyD (Type.toMyType argType)
      val argBind = (arg,argRefTy)
      val extendedVE = VE.add ve (arg, toRefTyS $ toRefSS argRefTy)
      (*
       * Γ[arg ↦ argTy] ⊢ body => bodyTy
       *)
      val bodyRefSS = typeSynthExp (extendedVE, re, body)
      val refSS = RefSS.substRefTy (bodyRefSS, RefTy.Arrow 
        (argBind, RefSS.toRefTy bodyRefSS))
    in
      refSS
    end

  and typeCheckLambda (ve : VE.t, re: RE.t, lam : Lambda.t, 
      ty : RefTy.t) : unit =
    let
      val (argBind as (_,argRefTy),resRefTy) = case ty of 
          RefTy.Arrow v => v
        | _ => Error.bug "Function with non-arrow type"
      val {arg,argType,body} = Lambda.dest lam
      val extendedVE = VE.add ve (arg, toRefTyS $ toRefSS argRefTy)
      (*
       * Substitute formal vars with actual vars
       *)
      val (binds, substs) = unifyArgs (argBind, (Exp.Val.Atom 
        (Exp.Val.Var (arg, Vector.new0 ()))))
      val _ = assert (Vector.length binds = 1, "Unification \
        \ of fn args incorrect.")
      val resRefTy' = RefTy.applySubsts substs resRefTy
    in
      (*
       * Γ[arg↦argRefTy] ⊢ body <= resRefTy
       *)
      typeCheckExp (extendedVE, re, body, resRefTy')
    end

  and typeCheckExp (ve : VE.t, re : RE.t, exp: Exp.t, ty: RefTy.t) 
      : unit =
    case Exp.node exp of
      Exp.Lambda l => typeCheckLambda (ve, re, l, ty)
    | Exp.Case {test:Exp.Val.t,rules,...} => Vector.foreach (rules, 
      fn ({pat,exp,...}) =>
        let
          val valbind = Dec.PatBind (pat,test)
          val extendedVE = doItValBind (ve, re,
            Vector.new0(),valbind)
        in
          typeCheckExp (extendedVE,re,exp,ty)
        end)
    | Exp.Let (decs,subExp) => 
      let
        val extendedVE = doItDecs (ve,re,decs)
      in
        typeCheckExp (extendedVE, re, subExp, ty)
      end
    | _ => 
      let
        val tySS = toRefSS ty
        (*
         * Γ ⊢ exp => expRefTy
         *)
        val expRefSS = typeSynthExp (ve, re, exp)
        (*
         * Γ ⊢ expRefTy <: ty
         *)
      in
        ignore $ VC.solveTypeCheck (ve, re, expRefSS, tySS)
      end

  and doItValBind (ve,re,tyvars,valbind) : VE.t = 
    case valbind of
      Dec.ExpBind (patval,exp) =>
      let
        val expss = typeSynthExp (ve,re,exp)
      in
         elabPatVal (ve,re,tyvars,patval,expss)
      end
    | Dec.PatBind (pat,expval) =>
      let
        val patnode = Pat.node pat
        (*  unimpl : Exp.Val.ty *)
        val expss = typeSynthExp (ve,re,Exp.make (Exp.Value expval,
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
          Pat.Value patval => 
            elabPatVal (ve,re,tyvars,patval,expss)
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
            val conSS  = RefTyS.instantiate (conTyS,tydargs)
            val conTy = RefSS.toRefTy conSS
            val eraseRef = fn (RefTy.Base (v,t,p)) => RefTy.Base 
              (v,t,Predicate.truee())
            (*
             * extend ve with new bindings for matched arguments.
             *)
            val (ve',resSS) = case (conTy,arg) of 
                (RefTy.Base _,NONE) => (ve,conSS)
              | (RefTy.Base _, SOME _) => Error.bug "Arguments to nullary \
                  \ cons"
              | (RefTy.Arrow (_,conResTy), SOME (Pat.Val.Atom 
                (Pat.Val.Wild))) => (ve, RefSS.substRefTy 
                  (conSS, eraseRef conResTy))
              | (RefTy.Arrow (conArgBind,conResTy), SOME argPatVal) => 
                  let
                    val argExpVal = patValToExpVal argPatVal
                    val (argTyMap,substs) = unifyArgs (conArgBind,argExpVal)
                    val ve' = Vector.fold (argTyMap, ve, fn ((arg,refTy),ve) =>
                      VE.add ve $ (arg,RefTyS.generalize (tyvars, 
                        (* Conargs have no refinements *)
                        RefSS.fromRefTy refTy))) 
                    val conResTy' = RefTy.applySubsts substs conResTy
                  in
                    (ve', RefSS.substRefTy (conSS, conResTy'))
                  end
              | _ => Error.bug "Impossible cons args"
            (*
             * Extend ve' with a new dummy var to keep track of
             * relationship between matched arguments and rhsvar.
             *)
            val resTy = RefSS.toRefTy resSS
            val RefTy.Base (bv,td,p) = RefTy.alphaRenameToVar resTy rhsvar
            val _ = assert (Var.toString bv = Var.toString rhsvar, 
              "RefTy alpha rename incorrect")
            val newTyS = RefTyS.generalize (tyvars, RefSS.substRefTy 
              (resSS, RefTy.Base (genVar(), td, p)))
          in
            VE.add ve' (genVar(), newTyS)
          end
        end

  and doItDecs (ve : VE.t, re : RE.t, decs : Dec.t vector) : VE.t =
    let
      fun elabRecDecs (ve : VE.t) (tyvars : Tyvar.t vector)  decs = 
        Vector.fold (decs,ve, fn ({lambda : Lambda.t, var : Var.t},ve) =>
          let
            val {arg,argType,body} = Lambda.dest lambda
            val argTyD = Type.toMyType argType
            val bodyTyD = Type.toMyType $ Exp.ty body
            val funTyD = TyD.makeTarrow (argTyD,bodyTyD)
            val funSS = mergeTypeToSS re (funTyD, RefTyS.specialize
              $ VE.find ve var) handle (VE.VarNotFound _) => 
                RefSS.fromRefTy $ RefTy.fromTyD funTyD
            val funspec = RefTyS.generalize (tyvars,funSS)
            val _ = print "The sort scheme is\n"
            val _ = Control.message (Control.Top, fn _ =>
              RefTyS.layout funspec)
          in
            VE.add ve (var,funspec)
          end)

      fun doItDec (ve : VE.t, re : RE.t, dec : Dec.t) :  VE.t = 
        case dec of
          Dec.Fun {decs,tyvars} => 
            let
              val extendedVE = elabRecDecs ve (tyvars()) decs
              val _ =  Vector.foreach(decs, fn ({lambda,var}) => 
                let
                  val fss = RefTyS.specialize $ VE.find extendedVE var
                    handle (VE.VarNotFound _) => Error.bug "ImpossibleCase!"
                  (*
                   * For recursive function lambdas are checked against 
                   * user-provided type or trivial type.
                   *)
                  val RefSS.T {paramrefty, ...} = fss
                  val typedParams = RefSS.typedParams paramrefty
                  (*
                   * relvars must be concretized as uninterpreted 
                   * relations.
                   *)
                  val typedRelIds = Vector.map (typedParams, 
                    fn (r,sps) => (RelId.fromString $ 
                      RelVar.toString r, sps))
                  val extendedRE = Vector.fold (typedRelIds, re, 
                    fn ((rid,sps),re) => addURelToRE re (rid,sps))
                  val insts = Vector.map2 (typedParams,typedRelIds,
                    fn ((r,_),(rid,_)) => (r,RelLang.ieatomOfRel rid))
                  val fty = RefSS.instRelParams (insts,paramrefty)
                in
                  typeCheckLambda (extendedVE, extendedRE, 
                    lambda, fty)
                end)
            in
              extendedVE
            end
        | Dec.Val {rvbs,tyvars,vbs} =>
            let
              val rvbsVE = doItDec (ve,re,Dec.Fun {decs=rvbs,tyvars=tyvars})
              val vbsVE = Vector.fold (vbs,rvbsVE,fn ({valbind,...},ve) =>
                doItValBind (ve,re,tyvars(),valbind))
            in
              vbsVE
            end
        | _ => ve

      val extendedVE = Vector.fold (decs,ve, 
        fn (dec,ve) => doItDec (ve,re,dec))

    in
      extendedVE
    end

  fun doIt (ve, re, Program.T{decs}) = (ignore $ doItDecs (ve, re, decs);
    VC.printVCsToFile ())

end

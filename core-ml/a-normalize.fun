(**
  In this note, elaboration means A-Normalization elaboration.

  Elaborating Expressions
  =======================
  In case of replacing exp with an elaborated let-expression,
  no futher changes need to be done to preserve the scoping of
  tyvars. Eg: In
    let val tyvarseq pat = exp in ... end
  replacing exp with
    let val x = exp1 in exp2 end 
  does not need generalization of free tyvars as all of them are 
  already captured by the the tyvarseq.

  However, if we replace the entire expression with
    let val <?1> var1 = exp1
        val tyvarseq pat = [var1 <?2> /exp1] exp2
     in ... end
   then, 
   1. Free tyvars in exp1 that were previously bound in tyvarseq
      have to be replaced with fresh tyvars, which should then be
      generalized at hole <?1>.
   2. Fresh tyvars bound in <?1> have to be instantiated with tyvars
      from tyvarseq. <?2> is filled with these tyvars.

  Elaborating Patterns
  ====================
  When replacing pats with atomic pats, the method is little 
  different from the above transformation. For eg, while replacing
    let val tyvarseq pat1 = exp1 in ... end
  with
    let val <?1> [x / pat2] pat1 = exp1
        val <?2> pat2 = x <?3>
    in ... end
  <?1> is tyvarseq (since type of x in pat1 is same as pat2). <?2>
  is tyvarseq, where each tyvar is replaced with fresh tyvar and
  <?3> is same as <?2>. Intuitively, we expect <?2> to contain fresh
  tyvars for only those tyvars from tyvarseq that occur free in pat1.
  But this is not the case as tyvars for all varibles defined through
  pat1 are bound in tyvarseq.
**)

functor ANormalize (S : A_NORMALIZE_STRUCTS) : A_NORMALIZE = 
struct
  open S
  structure C = CoreML
  structure A = ANormalCoreML
  structure Var = A.Var
  structure Tyvar = A.Tyvar

  local
    open A
  in
    type valbind = {exp: Exp.t, lay: unit -> Layout.t, 
        nest: string list, pat: Pat.t}
  end

  val symbase = "anc_"

  val count = ref 1024

  val genVar = fn _ => 
    let val id = symbase ^ (Int.toString (!count))
        val _ = count := !count + 1
    in
      Var.fromString id 
    end

  fun decsFromValBinds (tyvars : unit -> Tyvar.t vector) 
      (vbs : valbind list) : A.Dec.t vector =
    let
      val decFromValBind = fn vb => A.Dec.Val {
        rvbs = Vector.fromList [],
        tyvars = tyvars, 
        vbs = Vector.fromList [vb]}
    in
      Vector.fromList (List.map (vbs,decFromValBind))
    end

  val varToValue = fn var => 
    A.Value.Atom (A.Value.Var var)

  fun getValBindForExp (exp : A.Exp.t) (tyvars : Tyvar.t vector) :
    (A.Dec.t * Var.t) =
    let
      open A
      val expty = Exp.ty exp
      val newvar = genVar ()
      val newtyvars = Vector.map (tyvars, fn tyvar =>
        Tyvar.newNoname {equality = (Tyvar.isEquality tyvar)})
      val subpatnode = Pat.Value (Value.Atom (Value.Var newvar))
      val subpat = Pat.make (subpatnode,expty)
      val vbs = Vector.fromList [{exp = exp, 
        lay = fn _ => Layout.empty, nest = [], pat = subpat}]
      val dec = Dec.Val ({rvbs = Vector.fromList [],
        tyvars = fn _ => newtyvars, vbs = vbs})
    in
      (dec, newvar)
    end


  fun doItExp (exp : C.Exp.t) : (valbind list * A.Exp.t) = 
    (*Vector.fromList [], A.Lambda.make (C.Lambda.dest exp)*)
    raise (Fail "notimpl")

  fun getValBindForPat (pat : A.Pat.t) (tyvars : Tyvar.t vector): 
      (Var.t * A.Dec.t) =
    let
      open A
      val patty = Pat.ty pat
      val newvar = genVar ()
      val newtyvars = Vector.map (tyvars, fn tyvar =>
        Tyvar.newNoname {equality = (Tyvar.isEquality tyvar)})
      val subexp = Exp.make (Exp.Var (fn _ => newvar,
        fn _ => Vector.map (newtyvars, fn tyv => Type.var tyv)), 
          patty)
      val spatvb = Vector.fromList [{exp = subexp, 
        lay = fn _ => Layout.empty, nest = [], pat = pat}]
      val spatdec = Dec.Val ({rvbs = Vector.fromList [],
        tyvars = fn _ => newtyvars, vbs = spatvb})
    in
      (newvar, spatdec)
    end

  fun doItPatToVar (pat : C.Pat.t) (tyvars : Tyvar.t vector) : 
      (A.Value.atom * A.Dec.t list) =
    let
      open A
      val patty = C.Pat.ty pat
      val (spat,sspatdecs) = doItPat pat tyvars
      val spatty = Pat.ty spat
      val (atom,spatdecs) = case Pat.node spat of
          Pat.Value (Value.Atom v) => (v, sspatdecs)
        | _ => 
          let
            val (newvar, spatdec) = getValBindForPat spat tyvars
            val atom = Value.Var newvar
          in
            (atom,spatdec::sspatdecs)
          end
    in
      (atom, spatdecs)
    end

  and doItPatToAtomicPat (pat : C.Pat.t) (tyvars : Tyvar.t vector) : 
      (A.Value.t * A.Dec.t list) =
    let
      open A
      val patty = C.Pat.ty pat
      val (spat,sspatdecs) = doItPat pat tyvars
      val spatty = Pat.ty spat
      val (value,spatdecs) = case Pat.node spat of
          Pat.Value v => (v, sspatdecs)
        | _ => 
          let
            val (newvar, spatdec) = getValBindForPat spat tyvars
            val value = Value.Atom (Value.Var newvar)
          in
            (value,spatdec::sspatdecs)
          end
    in
      (value, spatdecs)
    end


  (*
   * doItPat takes complex pattern of form C(C<x>), splits it
   * and returns (C(t0), [{rvbs = [], tyvars = newtyvarseq, 
   *  vbs = [{pat=C<x>, exp = t0(newtyvarseq), lay = newlay,
   *    nest = ""}] }]).
   * The type of (exp t0) should be same as type of pattern C<x>.
   * Length of newtyvarseq = Length of tyvars.
   *)
  and doItPat (pat : C.Pat.t) (tyvars : Tyvar.t vector) : 
      (A.Pat.t * A.Dec.t list)  = 
    case C.Pat.node pat of
        C.Pat.Con ({arg = NONE, con, targs}) => 
          let
            val patty = C.Pat.ty pat
            val patnode = A.Pat.Con ({arg = NONE, con = con, 
              targs = targs})
          in
            (A.Pat.make (patnode,patty),[])
          end
      | C.Pat.Con ({arg = SOME arg,con,targs}) => 
        let
          open A
          val dummy = fn (c:Var.t) => c
          val _ = fn (c:Con.t) => dummy c
          val patty = C.Pat.ty pat
          val (value,subpatdecs) = doItPatToAtomicPat arg tyvars 
          val pat = Pat.Con ({arg = SOME value, con = con, 
            targs = targs})
        in
          (Pat.make (pat,patty), subpatdecs)
        end
      | C.Pat.Layered (var,pat') =>
        let
          val patty = C.Pat.ty pat
          val (atom,spatdecs) = doItPatToVar pat' tyvars
          val patnode = A.Pat.Layered (var,A.Value.Atom atom)
        in
          (A.Pat.make (patnode,patty), spatdecs)
        end
      | C.Pat.List argvec =>
        let
          open A
          val patty = C.Pat.ty pat
          val (atomlist, subpatdecslist) = Vector.foldr (argvec, ([],[]), 
            fn (arg, (atoms,spatdecslist)) => 
              let
                val (atom,spatdecs) = doItPatToVar arg tyvars
              in
                (atom::atoms, spatdecs::spatdecslist)
              end)
          val patnode = Pat.List (Vector.fromList atomlist)
        in
          (Pat.make (patnode,patty),List.concat subpatdecslist)
        end
      | C.Pat.Record (patrec) => 
        let
          open A
          val patty = C.Pat.ty pat
          val (atomlist, subpatdecslist) = Vector.foldr(Record.toVector patrec, 
            ([],[]), fn ((lbl,pat), (atoms,spatdecslist)) =>
              let
                val (atom,spatdecs) = doItPatToVar pat tyvars
              in
                ((lbl,atom)::atoms, spatdecs::spatdecslist)
              end)
          val atomrec = Record.fromVector (Vector.fromList atomlist)
          val patnode = Pat.Value (Value.Record atomrec)
        in
          (Pat.make (patnode,patty), List.concat subpatdecslist)
        end
      | C.Pat.Tuple argvec =>
        let
          open A
          val patty = C.Pat.ty pat
          val (atomlist, subpatdecslist) = Vector.foldr (argvec, ([],[]), 
            fn (arg, (atoms,spatdecslist)) => 
              let
                val (atom,spatdecs) = doItPatToVar arg tyvars
              in
                (atom::atoms, spatdecs::spatdecslist)
              end)
          val patnode = Pat.Value (Value.Tuple (Vector.fromList atomlist))
        in
          (Pat.make (patnode,patty),List.concat subpatdecslist)
        end
      | C.Pat.Var var => 
        let
          val patty = C.Pat.ty pat
          val patnode = A.Pat.Value (A.Value.Atom (A.Value.Var var))
        in
          (A.Pat.make (patnode,patty), [])
        end
      | C.Pat.Wild => (A.Pat.make (A.Pat.Wild, C.Pat.ty pat),[])

  fun doItLambda lam = 
    let
      val {arg,argType,body,mayInline} = C.Lambda.dest lam
      val bodyTy = C.Exp.ty body
      val (newvbs,newexp) = doItExp body
      (*
       * All the tyvars free in bodyexp are already bound 
       * at the enclosing dec for this lambda. No need to
       * generalize any tyvars.
       *)
      val tyvars = fn _ => Vector.fromList []
      val newbody = case newvbs of
          [] => newexp 
        | _ =>  A.Exp.make (A.Exp.Let (decsFromValBinds tyvars newvbs,
            newexp),bodyTy)
    in
      A.Lambda.make {arg = arg, argType = argType, 
        body = newbody}
    end

  fun doItDec (dec:C.Dec.t) : A.Dec.t list = case dec of
      C.Dec.Datatype r => [A.Dec.Datatype r]
    | C.Dec.Exception r => [A.Dec.Exception r]
    | C.Dec.Fun {decs = lamvec, tyvars} =>
        let val lv' = Vector.map (lamvec, fn {lambda,var} => 
          {lambda = doItLambda lambda,var=var})
        in
          [A.Dec.Fun {decs = lv',tyvars = tyvars}]
        end
    | C.Dec.Val {rvbs, vbs, tyvars, ...} =>
        let 
          val rvbs' = Vector.map (rvbs, fn {lambda,var} => 
            {lambda = doItLambda lambda, var = var})
          (*
           * for each vb in vbs, generate a vb' with simplified pattern
           * and simplified expression, while extracting subexpvbs and 
           * subpatvbs
           *)
          val  (subexpvbs, vbs', subpatdecs)= Vector.fold (vbs, ([],[],[]),
            fn ({exp,lay,nest,pat, ...},(pre, cur, post)) => 
              let
                val (subexpvbs,newexp) = doItExp exp
                val (newpat,spatdecs) = doItPat pat (tyvars())
                val vb' = {exp = newexp, lay = lay, nest = nest,
                  pat = newpat}
              in
                (List.concat [subexpvbs,pre], 
                 List.concat [cur,[vb']],
                 List.concat [spatdecs,post])
              end)
          val subexpdecs = List.map (subexpvbs, fn subexpvb => 
            A.Dec.Val ({rvbs = Vector.fromList [], tyvars = tyvars, 
              vbs = Vector.fromList [subexpvb]}))
          val expdec = [A.Dec.Val ({rvbs = rvbs', tyvars = tyvars,
              vbs = Vector.fromList vbs'})]
        in
          List.concat [subexpdecs, expdec, subpatdecs]
        end

  val doIt = fn (C.Program.T{decs}) => 
    let
      val decss = Vector.toListMap (decs,doItDec)
      val decsv = Vector.fromList (List.concat decss)
    in
      A.Program.T{decs = decsv}
    end
end

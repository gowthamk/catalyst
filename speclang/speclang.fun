functor SpecLang (S : SPEC_LANG_STRUCTS) : SPEC_LANG = 
struct
  open S
  structure L = Layout
  structure TyD = TypeDesc

  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  val fst = fn (x,y) => x
  fun varStrEq (v1,v2) = (Var.toString v1 = Var.toString v2)
  fun varSubst (subst as (new,old)) v = if varStrEq (v,old) 
    then new else v
  val tyVarEq = fn (v1,v2) => (Tyvar.toString v1 = Tyvar.toString v2)
  fun instTyvars (eqs, t, mapTyD) =
    let
      fun inst v' = Vector.peekMap (eqs, fn (v,tyd) => 
        if tyVarEq (v,v') then SOME tyd else NONE)
      val mapf = fn tyd => TyD.mapTvar tyd (fn v =>
        case inst v of NONE => TyD.makeTvar v
          | SOME tyd' => tyd')
    in
      mapTyD t mapf
    end
  fun isValidInst (eqs, vars, isEq) = 
    let
      fun inst v' = Vector.peekMap (eqs, fn (v,x) => 
        if isEq (v,v') then SOME x else NONE)
    in
      Vector.forall (vars, fn v => 
        case inst v of SOME _ => true
        | NONE => false)
    end

  structure RelId = Var

  structure RelVar = 
  struct
    open Var
    val eq = fn (rv1,rv2) => (toString rv1 = toString rv2)
  end

  structure RelTyvar =
  struct
    type t = Var.t

    val symbase = "'T"

    val count = ref 0

    val new = fn _ => 
      let val id = symbase ^ (Int.toString (!count))
          val _ = count := !count + 1
      in
        Var.fromString id 
      end

    fun eq (v1,v2) = (Var.toString v1 = Var.toString v2)
    val toString = Var.toString 
  end

  structure RelType =
  struct
    (*
     * Type of rexpr is always a tuple.
     * Type is empty tuple if and only if rexpr is {()}
     *)
    datatype t = Tuple of TypeDesc.t vector
               | Reltyvar of RelTyvar.t
               | Cross of t * t

    fun toString (Tuple tydv) = Vector.toString 
      TypeDesc.toString tydv
      | toString (Reltyvar t) = RelTyvar.toString t
      | toString (Cross (t1,t2)) = (toString t1) ^ "*" ^
          (toString t2)

    val toString = fn t => "{"^(toString t)^"}"

    (* trivial equality *)
    fun equal (Tuple tydv1, Tuple tydv2) = 
      (Vector.length tydv1 = Vector.length tydv2) andalso
      Vector.forall2 (tydv1,tydv2,TypeDesc.sameType)
      | equal (Reltyvar v1, Reltyvar v2) = (RelTyvar.toString v1) =
          (RelTyvar.toString v2)
      | equal (Cross (t1,t2), Cross (t3,t4)) = (equal (t1,t3))
          andalso (equal(t2,t4))
      | equal _ = false

    fun newTuple tv = Tuple tv
    fun newVar rv = Reltyvar rv
    val empty = Tuple $ Vector.new0 ()
      
    fun crossPrdType (t1,t2) = case (t1,t2) of
        (Tuple tyds1, Tuple tyds2) =>
        (case (Vector.length tyds1, Vector.length tyds2) of
          (0,_) => empty | (_,0) => empty)
      | (Tuple tyds1,_) => if Vector.length tyds1 = 0 then empty
          else Cross (t1,t2)
      | (_,Tuple tyds2) => if Vector.length tyds2 = 0 then empty
          else Cross (t1,t2)
      | _ => Cross (t1,t2)

    fun mapTyD (Tuple tv) f = Tuple $ Vector.map (tv,f)
      | mapTyD (Cross (t1,t2)) f = Cross (mapTyD t1 f, mapTyD t2 f)
      | mapTyD t f = t

    fun foldTyD (Tuple tv) b f = Vector.fold (tv,b,f)
      | foldTyD (Cross (t1,t2)) b f = foldTyD t1 (foldTyD t2 b f) f
      | foldTyD (Reltyvar _) b f = b

    fun mapRelTyVar t f = 
      let
        val doIt = fn t => mapRelTyVar t f
      in
        case t of Reltyvar v => f v
        | Cross (t1,t2) => Cross (doIt t1, doIt t2)
        | Tuple _ => t
      end

    fun foldRelTyVar t b f = 
      let
        val doIt = fn t => fn b => foldRelTyVar t b f
      in
        case t of Reltyvar v => f (v,b)
        | Cross (t1,t2) => doIt t1 $ doIt t2 b
        | Tuple _ => b

      end 

    val instTyvars = fn (eqs,t) => instTyvars (eqs,t,mapTyD)

    fun instRelTyvars (eqs,t) = 
      let
        fun inst v' = Vector.peekMap (eqs, fn (v,rt) => 
          if RelTyvar.eq (v,v') then SOME rt else NONE)
      in
        mapRelTyVar t (fn v => case inst v of 
          SOME rt => rt | NONE => newVar v)
      end 
  end

  fun instRelTyvars (eqs, t, mapRelTy) = mapRelTy t (fn rt =>
    RelType.instRelTyvars (eqs,rt))

  structure RelTyConstraint =
  struct
    structure TyD = TypeDesc

    datatype t = Equal of RelType.t * RelType.t

    type sol = (RelTyvar.t * RelType.t)

    fun new (rt1,rt2) = Equal (rt1,rt2)

    fun eq (Equal x) = x

    val relTyVarEq = RelTyvar.eq

    fun mapTyD (Equal (rt1,rt2)) f = new (RelType.mapTyD rt1 f, 
      RelType.mapTyD rt2 f)

    fun mapRelTy (Equal (rt1,rt2)) f = new (f rt1, f rt2)

    fun foldTyD (Equal (rt1,rt2)) b f = RelType.foldTyD rt1
      (RelType.foldTyD rt2 b f) f

    fun foldRelTy (Equal (rt1,rt2)) b f = f (rt1, f (rt2, b))

    fun delegateInst g (eqs,cs) = Vector.map (cs, fn c => 
      mapRelTy c (fn rt => g (eqs,rt)))

    val instTyvars = delegateInst RelType.instTyvars

    val instRelTyvars = delegateInst RelType.instRelTyvars

    val relTyVarsIn =  fn rt => Vector.fromList $ 
      RelType.foldRelTyVar rt [] (fn (v,acc) => v::acc)

    fun unifyRelTypes (rt1,rt2) = 
      let
        val rc = fn _ => new (rt1,rt2)
        val rt = fn _ => 
          let
            val n1 = Vector.length $ relTyVarsIn rt1
            val n2 = Vector.length $ relTyVarsIn rt2
          in
            if n2 < n1 then rt2 else rt1
          end
        open RelType
      in
        case (rt1,rt2) of
          (Tuple tyds1, Tuple tyds2) =>
          (case (Vector.length tyds1, Vector.length tyds2) of
            (0,_) => (NONE, rt2) | (_,0) => (NONE,rt1))
        | (Tuple tyds1,_) => if Vector.length tyds1 = 0 
            then (NONE, rt2) 
            else (SOME $ rc (), rt ())
        | (_,Tuple tyds2) => if Vector.length tyds2 = 0 
            then (NONE, rt1) 
            else (SOME $ rc (), rt ())
        | _ => (SOME $ rc (), rt ())
      end 

    fun assertCompatible (tv1 : TyD.t vector,tv2) =
      Vector.foreach2 (tv1,tv2, fn (tyd1,tyd2) => 
        assert (TyD.sameType (tyd1,tyd2),
          "Incompatible tuple types in rexpr"))

    fun trySolveConstraint (c : t) : sol option =
      let
        open RelType
        fun assertNotCirc (v,rt) = 
          let
            val rhsvs = relTyVarsIn rt
            val _ = Vector.foreach (rhsvs, fn rhsv =>
              assert (not $ relTyVarEq (v,rhsv), "Circular\
                \ relty constraint. Unsolvable."))
          in
            ()
          end
        val eqOp = case eq c of
            (Reltyvar v1, rty2) => (assertNotCirc (v1,rty2);
              SOME (v1,rty2))
          | (rty1, Reltyvar v2) => (assertNotCirc (v2,rty1);
              SOME (v2,rty1))
          | _ => NONE
      in
        eqOp
      end
   
    (*
     * Simplifies constraints using some rules. 
     * More precision can be obtained by strengthening this function.
     * Rest of the functions remain same.
     *)
    fun simplify (cs : t vector) =
      let
        open RelType
        fun doIt c  = case eq c of
            (Cross (Tuple tv1,Reltyvar v1), 
             Cross (Tuple tv2,Reltyvar v2)) => 
            let
              val l1 = Vector.length tv1
              val l2 = Vector.length tv2
              val diff = l1-l2
              val (tv1,tv2,(v,rty)) = if diff = 0 
                then (tv1, tv2, (v1, Reltyvar v2))
                else if diff < 0 
                then  (tv1, Vector.prefix (tv2,l1), (v1, 
                  Cross (newTuple $ Vector.dropPrefix (tv2,l1), 
                         Reltyvar v2)))
                else (Vector.prefix (tv1,l2), tv2, (v2, 
                  Cross (newTuple $ Vector.dropPrefix (tv1,l2), 
                         Reltyvar v1)))
              val _ = assertCompatible (tv1,tv2)
            in
              new (newVar v,rty)
            end
          | (Cross (Reltyvar v1, Tuple tv1), 
             Cross (Reltyvar v2, Tuple tv2)) => 
            let
              val l1 = Vector.length tv1
              val l2 = Vector.length tv2
              val diff = l1-l2
              val (tv1,tv2,(v,rty)) = if diff = 0 
                then (tv1, tv2, (v1, Reltyvar v2))
                else if diff < 0 
                then  (tv1, Vector.dropPrefix (tv2, l2 - l1), (v1, 
                  Cross (Reltyvar v2, newTuple $ 
                         Vector.prefix (tv2, l2 - l1))))
                else (Vector.dropPrefix (tv1,diff), tv2, (v2, 
                  Cross (Reltyvar v1, newTuple $ 
                         Vector.prefix (tv1,diff) )))
              val _ = assertCompatible (tv1,tv2)
            in
              new (newVar v,rty)
            end
          | (Cross (Reltyvar v1, Tuple tv1), Tuple tv2) =>
            let
              val l1 = Vector.length tv1
              val l2 = Vector.length tv2
              val _ = assert(l1<l2, "Incompatible rexpr types")
              val _ = assertCompatible (tv1, 
                Vector.dropPrefix (tv2, l2-l1))
            in
              new (newVar v1, Tuple $ Vector.prefix (tv2,l2-l1))
            end
          | (Tuple tv2, Cross (Reltyvar v1, Tuple tv1)) =>
            let
              val l1 = Vector.length tv1
              val l2 = Vector.length tv2
              val _ = assert(l1<l2, "Incompatible rexpr types")
              val _ = assertCompatible (tv1, 
                Vector.dropPrefix (tv2, l2-l1))
            in
              new (newVar v1, Tuple $ Vector.prefix (tv2,l2-l1))
            end
          | (Cross (Tuple tv1, Reltyvar v1), Tuple tv2) =>
            let
              val l1 = Vector.length tv1
              val l2 = Vector.length tv2
              val _ = assert(l1<l2, "Incompatible rexpr types")
              val _ = assertCompatible (tv1, 
                Vector.prefix (tv2, l2-l1))
            in
              new (newVar v1, Tuple $ Vector.dropPrefix (tv2,l2-l1))
            end
          | (Tuple tv2, Cross (Tuple tv1, Reltyvar v1)) =>
            let
              val l1 = Vector.length tv1
              val l2 = Vector.length tv2
              val _ = assert(l1<l2, "Incompatible rexpr types")
              val _ = assertCompatible (tv1, 
                Vector.prefix (tv2, l2-l1))
            in
              new (newVar v1, Tuple $ Vector.dropPrefix (tv2,l2-l1))
            end
          | _ => c
      in
        Vector.map (cs,doIt)
      end
   
    fun clearTautologies (cs : t vector) =
      let
        open RelType 
      in
        Vector.keepAllMap (cs, fn c => case eq c of
          (Tuple tv1,Tuple tv2) => (assertCompatible (tv1,tv2);
            NONE)
        | (Reltyvar v1, Reltyvar v2) => if relTyVarEq (v1,v2) 
            then NONE else SOME c
        | (Cross (Reltyvar v1,rt1), Cross (Reltyvar v2,rt2)) =>
            if relTyVarEq (v1,v2) then SOME $ new (rt1,rt2) else SOME c
        | (Cross (rt1, Reltyvar v1), Cross (rt2, Reltyvar v2)) =>
            if relTyVarEq (v1,v2) then SOME $ new (rt1,rt2) else SOME c
        | _ => SOME c)
      end

    (*
     * Applies reltyvar eqn (v=rt) in cs. 
     * Post-condition : v does not occur in cs. 
     *)
    fun applyRelTyVarEqn (eqn as (v,rt)) (cs :t vector) : t vector =
      instRelTyvars (Vector.new1 eqn,cs)
    
    (*
     * Solves a constraint, applies the solution to the rest, and
     * clears any new tautologies. Repeats this process until there are
     * no more constraints, or residue is unsolvable.
     * Invariant : 
     * sol : {v:Reltyvar} -> {rt:RelType.t | v notin RelType.relTyVarsIn rt}
     * , is a partial function.
     *)
    fun relTyVarEqns (cs : t vector) : (sol vector * 
        (t vector)) = 
      let
        open RelType
        val cs = simplify $ clearTautologies cs
        val solEqnOp = Vector.loop (cs, 
          fn c => case trySolveConstraint c of
              SOME sol => SOME $ SOME sol
            | _ =>NONE, 
          fn () => NONE)
        val (sol,residue) = case solEqnOp of 
          NONE => (Vector.new0 (), cs)
        | SOME solEqn => 
          let
            val newcs = applyRelTyVarEqn solEqn cs
            val (moreEqns,residue) = relTyVarEqns newcs
            val sol = Vector.concat [Vector.new1 solEqn, moreEqns]
          in
            (sol,residue)
          end
      in
        (sol,residue)
      end
   
    (*
     * solveRelTyVarEqns : eqs -> newrtyvars -> sol
     * Pre-condition : eqs is a partial function without circular
     * equations, as described previously.
     * Post-condition : RelTyVarsIn(range of sol) intersection
     * (domain of sol) = empty
     *)
    fun solveRelTyVarEqns (sol : sol vector) newrtyvs =
      let
        val domain = #1 $ Vector.unzip sol
        fun notInDomain v = Vector.forall (domain,
          fn v' => not (relTyVarEq (v,v')))
        fun notNewRTyVar v = List.forall (newrtyvs,
          fn v' => not (relTyVarEq (v,v')))
        val rtyvOp = Vector.loop (sol, 
          fn (_,rty) => case Vector.peek (relTyVarsIn rty,
            fn v => notNewRTyVar v andalso notInDomain v) of
              SOME v => SOME $ SOME v
            | NONE => NONE,
          fn _ => NONE) 
      in
        case rtyvOp of NONE => sol
        | SOME rtyv => 
          let
            val newrtyv = RelTyvar.new ()
            val newEq = (rtyv, RelType.newVar newrtyv)
            (* 
             * This doesn't type check:
             * val sol' = clearTautologies $ applyRelTyVarEqn newEq sol
             * Moreover, clearTautologies is not needed. There won't be
             * any tautologies as eqs is a partial function. 
             * Also, we only ever apply newEq in the range of
             * sol, which is a RelType.t. This is because newEq is
             * [v := newv], where v occurs only in the RHS of sol.
             *)
            val sol' = Vector.map (sol, fn (v,rty) => (v,
              RelType.instRelTyvars (Vector.new1 newEq, rty)))
            val newSol = Vector.concat [Vector.new1 newEq, sol']
          in
            solveRelTyVarEqns newSol (newrtyv :: newrtyvs)
          end
      end

    val solveRelTyVarEqns = fn sols => solveRelTyVarEqns sols []
    (*
     * Solves constraints and returns solution.
     * Solution is a partial function over reltyvars of cs, such that 
     * reltyvars in its range are disjoint with its domain.
     * Also returns residual constraints that it could not solve.
     * The problem, in general, is undecidable. For eg,
     * T1 X T2 = T3 X T4 cannot be solved.
     *)
    fun solvePartial (cs : t vector) : (sol vector * 
        (t vector)) =
      let
        open RelType
        val (rtvEqns, residue) =  relTyVarEqns cs
        val solEqns = solveRelTyVarEqns rtvEqns
        val residue = Vector.fold (solEqns, residue,
          fn (solEq,resacc) => applyRelTyVarEqn solEq resacc)
      in
        (solEqns,residue)
      end
  end

  structure RelTyC = RelTyConstraint

  structure SimpleProjSort =
  struct
    datatype t = Base of RelType.t
               | ColonArrow of TypeDesc.t * RelType.t

    fun toString (ColonArrow (tyD,relKind)) =
      (TypeDesc.toString tyD)^" :-> "^(RelType.toString relKind)
      | toString (Base rt) = RelType.toString rt

    fun layout t = L.str $ toString t

    fun mapTyD (Base rt) f = Base (RelType.mapTyD rt f)
      | mapTyD (ColonArrow (tyd,rt)) f = ColonArrow (f tyd, 
          RelType.mapTyD rt f)

    fun foldTyD (Base rt) b f = RelType.foldTyD rt b f
      | foldTyD (ColonArrow (tyd,rt)) b f = f (tyd, 
            RelType.foldTyD rt b f)

    fun mapRelTy (Base rt) f = Base (f rt)
      | mapRelTy (ColonArrow (tyd,rt)) f = 
          ColonArrow (tyd, f rt)

    fun foldRelTy (Base rt) b f = f (rt,b)
      | foldRelTy (ColonArrow (_,rt)) b f = f (rt,b)
  
    fun newBase rt = Base rt

    fun newColonArrow (tyd,rt) = ColonArrow (tyd,rt)

    fun unify (Base rt1, Base rt2) = RelTyConstraint.new (rt1,rt2)
      | unify (ColonArrow (tyd1,rt1), ColonArrow (tyd2,rt2)) =
          (assert (TypeDesc.sameType (tyd1,tyd2), "Domains of projections\
            \ not unifiable");
           RelTyConstraint.new (rt1,rt2))
      | unify _ = raise (Fail "Projection expected to be 0th order\
          \ in one case, and 1st order in another")

    val instTyvars = fn (eqs,t) => instTyvars (eqs,t,mapTyD)

    val instRelTyvars = fn (eqs,t) => instRelTyvars (eqs,t,mapRelTy)

    fun domain (ColonArrow (d,_)) = d
      | domain _ = raise (Fail "No domain for base rel type")

    fun range (ColonArrow (_,r)) = r
      | range _ = raise (Fail "No range for base rel type")

  end

  structure SPS = SimpleProjSort

  structure ProjSort =
  struct
    datatype t =  T of {paramsorts : SimpleProjSort.t vector,
                        sort : SimpleProjSort.t}
    fun toString (T {paramsorts,sort}) = (Vector.toString
        SimpleProjSort.toString paramsorts) 
      ^ " -> " ^ (SimpleProjSort.toString sort)

    fun new (paramsorts, sort) = T {paramsorts = paramsorts,
      sort = sort}

    fun domain (T{sort, ...}) = SimpleProjSort.domain sort

    fun range (T{sort, ...}) = SimpleProjSort.range sort

    fun paramSorts (T {paramsorts,...}) = paramsorts

    fun foldWith g (T{paramsorts,sort}) b f = 
      g sort (Vector.fold (paramsorts, b, fn (p,acc) => 
        g p acc f)) f

    fun foldTyD t b f = foldWith SimpleProjSort.foldTyD t b f

    fun foldRelTy t b f = foldWith SimpleProjSort.foldRelTy t b f

    fun instTyvars (eqs,T{paramsorts,sort}) =
      T {paramsorts = Vector.map (paramsorts,
            fn ps => SPS.instTyvars (eqs,ps)),
         sort = SPS.instTyvars (eqs,sort)}

    fun instRelTyvars (eqs,T{paramsorts,sort}) =
      T {paramsorts = Vector.map (paramsorts,
            fn ps => SPS.instRelTyvars (eqs,ps)),
         sort = SPS.instRelTyvars (eqs,sort)}
  end

  structure ProjSortScheme =
  struct
    datatype t = T of {reltyvars : RelTyvar.t vector,
                      constraints: RelTyConstraint.t vector,
                            sort : ProjSort.t}

    fun toString (T{reltyvars,constraints,sort}) = 
      (Vector.toString RelTyvar.toString reltyvars)^" "^
        (ProjSort.toString sort)

    fun specialize (T{sort, ...}) = sort

    fun domain (T{sort, ...}) = ProjSort.domain sort

    fun foldTyD (T{constraints,sort,...}) b f = 
      ProjSort.foldTyD sort (Vector.fold (constraints, b,
        fn (c,acc) => RelTyC.foldTyD c acc f)) f

    fun foldRelTy (T{constraints,sort,...}) b f = 
      ProjSort.foldRelTy sort (Vector.fold (constraints, b,
        fn (c,acc) => RelTyC.foldRelTy c acc f)) f

    fun generalize (cstrs,sort) =
      let
        val {add, ...} = List.set {equals = RelTyvar.eq, 
          layout = (L.str o RelTyvar.toString) }
        val t = T {reltyvars = Vector.new0 (),
          constraints = cstrs, sort = sort}
        val reltyvars = Vector.fromList $ foldRelTy t [] 
          (fn (rt,acc1) => RelType.foldRelTyVar rt acc1 
            (fn (rv,acc2) => add (acc2,rv)))
      in
        T {reltyvars = reltyvars, constraints = cstrs, sort = sort}
      end
      
    fun instantiate (eqs, T {reltyvars, constraints, sort}) =
      let
        val _ = assert (isValidInst (eqs, reltyvars, RelTyvar.eq),
           "Reltyvar instantiation incomplete")
        val cs' = RelTyC.instRelTyvars (eqs,constraints)
        val sort' = ProjSort.instRelTyvars (eqs,sort)
      in
        (cs',sort')
      end

    fun instTyvars (eqs, T {reltyvars, constraints, sort})=
      let
        val cs' = RelTyC.instTyvars (eqs,constraints)
        val sort' = ProjSort.instTyvars (eqs,sort)
      in
        T {reltyvars = reltyvars, constraints = cs', sort = sort'}
      end
  end

  structure ProjTypeScheme =
  struct
    datatype t = T of {tyvars : Tyvar.t vector,
                       sortscheme : ProjSortScheme.t}

    fun toString (T {tyvars, sortscheme}) = 
        (Vector.toString Tyvar.toString tyvars) 
      ^ ". " ^ (ProjSortScheme.toString sortscheme)

    fun generalize ss =
      let
        val {add, ...} = List.set {equals = tyVarEq,
          layout = (L.str o Tyvar.toString) }
        val tyvars = Vector.fromList $ 
          ProjSortScheme.foldTyD ss [] 
          (fn (tyd,acc1) => TyD.foldTvar tyd acc1
            (fn (tvar,acc2) => add (acc2,tvar)))
      in
        T {tyvars = tyvars, sortscheme = ss}
      end

    fun specialize (T{sortscheme, ...}) = sortscheme

    fun tyvars (T{tyvars, ...}) = tyvars
    
    fun domain (T{sortscheme, ...}) = ProjSortScheme.domain sortscheme

    fun instantiate (eqs, T{tyvars,sortscheme}) = 
      let
        val _ = assert (isValidInst (eqs, tyvars, tyVarEq),
           "Reltyvar instantiation incomplete")
      in
        ProjSortScheme.instTyvars (eqs,sortscheme)
      end
      
  end

  structure RelLang =
  struct

    datatype elem = Int of int
                  | Bool of bool
                  | Var of Var.t
    datatype instexpr = Relation of RelId.t
                      | Relvar of RelVar.t
                      | Inst of {args: ieatom vector,
                                  rel : RelId.t}
    and ieatom = Ie of instexpr
               | Re of expr
    and expr = T of elem vector
             | X of expr * expr
             | U of expr * expr
             | R1 of RelVar.t
             | R2 of instexpr * Var.t

    datatype term = Atom of ieatom
                  | Star of instexpr

    val elemToString = fn el => case el of
        Int i => Int.toString i
      | Bool b => Bool.toString b
      | Var v => Var.toString v
    
    fun instExprToString (Relation r) = RelId.toString r
      | instExprToString (Relvar rv) = RelVar.toString rv
      | instExprToString (Inst {args, rel}) = "(" ^ 
        (RelId.toString rel) ^ " " ^ (L.toString $ L.seq $ L.separate 
          (Vector.toListMap (args, 
            fn ieatom => L.str $ ieatomToString ieatom), " ")) 
          ^ ")"

    and ieatomToString (Ie ie) = instExprToString ie
      | ieatomToString (Re re) = exprToString re

    and exprToString exp = case exp of
        T (elvec) => "{(" ^ (Vector.fold (elvec,"",fn(e,acc) => 
          (elemToString e) ^ acc)) ^ ")}"
      | X (e1,e2) => "(" ^ (exprToString e1) ^ " X " 
          ^ (exprToString e2) ^ ")"
      | U (e1,e2) => "(" ^ (exprToString e1) ^ " U " 
          ^ (exprToString e2) ^ ")"
      | R1 (relvar) => (RelVar.toString relvar)
      | R2 (instexp,arg) => (instExprToString instexp) ^ "(" 
          ^ (Var.toString arg) ^ ")"
    
    val exprToString = exprToString

    val termToString = fn trm => case trm of
        Atom e => ieatomToString e
      | Star instexp => (instExprToString instexp) ^ "*"
    fun termOfExpr expr = Atom $ Re expr
    fun termOfIExpr ie = Atom $ Ie ie 
    fun instExprOfRel r = Relation r
    fun instExprOfRelInst (r,ieats) = case Vector.length ieats of
        0 => Relation r | _ => Inst {args = ieats, rel = r}
    fun instExprOfRelVar rv = Relvar rv
    fun ieatomOfInstExpr ie = Ie ie
    fun ieatomOfRel r = ieatomOfInstExpr $ instExprOfRel r
    fun ieatomOfRelVar rv = ieatomOfInstExpr $ instExprOfRelVar rv
    fun ieatomOfExpr rexpr = Re rexpr

    fun mapRelInstExpr (Relation rid) f = f rid
      | mapRelInstExpr (Inst {args,rel}) f = Inst {
          args = Vector.map (args, fn ieat => mapRelIEAtom ieat f),
          rel = rel}
      | mapRelInstExpr rv f = rv

    and mapRelIEAtom (Ie ie) f = Ie $ mapRelInstExpr ie f
      | mapRelIEAtom (Re expr) f = Re $ mapRelExpr expr f

    and mapRelExpr (X (e1,e2)) f = X (mapRelExpr e1 f, 
          mapRelExpr e2 f)
      | mapRelExpr (U (e1,e2)) f = U (mapRelExpr e1 f, 
          mapRelExpr e2 f)
      | mapRelExpr (R2 (ie,v)) f = R2 (mapRelInstExpr ie f, v)
      | mapRelExpr expr f = expr

    fun mapRel (Atom ieat) f = Atom $ mapRelIEAtom ieat f
      | mapRel (Star ie) f = Star $ mapRelInstExpr ie f

    fun ieMapRVarToExpr (Inst{args,rel}) f =
          Inst {args = Vector.map (args, fn arg => 
                       ieAtomMapRVarToExpr arg f),
                rel = rel}
      | ieMapRVarToExpr ie f = ie

    and ieAtomMapRVarToExpr ieat f = case ieat of
        Ie ie => Ie $ ieMapRVarToExpr ie f
      | Re re => Re $ mapRVarToExpr re f

    and mapRVarToExpr t (f : RelVar.t -> expr) : expr = 
      let
        val doIt = fn e => mapRVarToExpr e f
      in
        case t of
          X (e1,e2) => X (doIt e1, doIt e2)
        | U (e1,e2) => U (doIt e1, doIt e2)
        | R1 rv => f rv
        | R2 (ie,v) => R2 (ieMapRVarToExpr ie f ,v)
        | T _ => t
      end

    fun ieMapRVarToIExpr (Inst{args,rel}) f =
          Inst {args = Vector.map (args, fn arg => 
                       ieAtomMapRVarToIExpr arg f),
                rel = rel}
      | ieMapRVarToIExpr (Relvar rv) f = f rv
      | ieMapRVarToIExpr ie f = ie

    and ieAtomMapRVarToIExpr ieat f = case ieat of
        Ie ie => Ie $ ieMapRVarToIExpr ie f
      | Re re => Re $ mapRVarToIExpr re f

    and mapRVarToIExpr t (f : RelVar.t -> instexpr) : expr = 
      let
        val doIt = fn e => mapRVarToIExpr e f
      in
        case t of
          X (e1,e2) => X (doIt e1, doIt e2)
        | U (e1,e2) => U (doIt e1, doIt e2)
        | R2 (ie,v) => R2 (ieMapRVarToIExpr ie f ,v)
        | _ => t
      end

    fun ieInstRelVars (eqs, ie) : instexpr = Vector.fold (eqs, ie, 
      fn ((v,ieat), ie) => case ieat of
          Ie ie => ieMapRVarToIExpr ie (fn v' => 
            if RelTyvar.eq (v,v') then ie else Relvar v)
        | Re re => ieMapRVarToExpr ie (fn v' => 
            if RelTyvar.eq (v,v') then re else R1 v))

    and ieAtomInstRelVars (eqs, ieat) : ieatom = case ieat of
        Ie ie => Ie (ieInstRelVars (eqs, ie))
      | Re re => Re (instRelVars (eqs, re))

    and instRelVars (eqs, expr) = Vector.fold (eqs, expr, 
      fn ((v,ieat), expr) => case ieat of
          Ie ie => mapRVarToIExpr expr (fn v' => 
            if RelTyvar.eq (v,v') then ie else Relvar v)
        | Re re => mapRVarToExpr expr (fn v' => 
            if RelTyvar.eq (v,v') then re else R1 v))

    fun instRelVarsInTerm (eqs,Atom ieat) = Atom $ ieAtomInstRelVars
            (eqs, ieat)
      | instRelVarsInTerm (eqs,Star ie) = Star $ ieInstRelVars 
            (eqs, ie)

    fun app (relId,var) = R2 (relId,var)
    fun union (e1,e2) = U (e1,e2)
    fun crossprd (e1,e2) = X (e1,e2)
    fun emptyexpr _ = T (Vector.fromList [])
    fun applySubsts substs rexpr = 
      let
        val doIt = applySubsts substs
        (* caution : telescoped substitutions *)
        fun subst v = Vector.fold (substs, v, fn ((new,old),v) =>
          if (Var.toString old = Var.toString v) then new else v)
        fun elemSubst elem = case elem of
            Var v => Var (subst v)
          | c => c
        fun ieatomSubst (Ie ie) = Ie (ieSubst ie)
          | ieatomSubst (Re re) = Re (doIt re)
        and ieSubst (Inst {args,rel}) = Inst {args =
          Vector.map (args, ieatomSubst), rel = rel}
          | ieSubst ie = ie
      in
      case rexpr of 
          T elemv => T (Vector.map (elemv,elemSubst))
        | X (e1,e2) => X (doIt e1, doIt e2)
        | U (e1,e2) => U (doIt e1, doIt e2)
        | R1 rv => R1 rv
        | R2 (ie,argvar) => R2 (ieSubst ie, (subst argvar))
      end
  end

  structure Pat =
  struct
    datatype value = Var of Var.t
                   | Tuple of Var.t vector
                   | Record of Var.t Record.t
    datatype t = Con of Con.t * value option
               | Value of value
    fun valueToString (Var v) = Var.toString v
      | valueToString (Tuple vars) = Vector.toString Var.toString
          vars
      | valueToString (Record rc) = L.toString (Record.layout 
          { record = rc, 
            separator = ",", 
            extra = "", 
            layoutTuple = fn vars => L.str $ Vector.toString 
              Var.toString vars,
            layoutElt = fn v => L.str $ Var.toString v })

    fun toString (Con (c,patvalop)) = (Con.toString c) ^
        (case patvalop of NONE => "" 
          | SOME patval => " " ^ (valueToString patval))
      | toString (Value patval) = valueToString patval
  end

  structure StructuralRelation =
  struct
    datatype t = T of {id : RelId.t,
                       params : RelVar.t vector,
                       map : (Pat.t option * RelLang.term)
                             vector}

    fun new data = T data
    fun patMapToString map =
      let
        val patmap = "{" ^ (Vector.toString (fn (pato,trm) =>
            let
              val patstr = case pato of NONE => ""
                | SOME pat => Pat.toString pat
              val trmstr = RelLang.termToString trm
            in
              patstr ^ " => " ^ trmstr
            end) map) ^ "}\n"
      in
        patmap
      end
    
    fun instantiate (eqs, (T{id, params, map})) =
      let
        val _ = assert (isValidInst (eqs, params, RelVar.eq),
           "RelVar instantiation incomplete")
        val map' = Vector.map (map, fn (pato,rterm) =>
          (pato, RelLang.instRelVarsInTerm (eqs, rterm)))
      in
        map'
      end

    val toString = fn T{id,params,map} =>
      let
        val relstr = case Vector.length params of
          0 => RelId.toString id
        | _ => RelLang.instExprToString $
            RelLang.instExprOfRelInst (id, Vector.map 
              (params,RelLang.ieatomOfRel))
        val patmap = patMapToString map
      in
        "relation " ^ relstr ^ " = " ^ patmap
      end
  end

  structure TyDBinds =
  struct
    structure Key = 
    struct
      type t = Var.t
      val layout = L.str o Var.toString
      fun equal (v1,v2) = (Var.toString v1) = (Var.toString v2)
    end
    structure Map = ApplicativeMap (structure Key = Key
                                   structure Value = TypeDesc)
    open Map
  end

  structure Predicate =
  struct
    structure BasePredicate =
    struct
      datatype expr = Int of int
                    | Bool of bool
                    | Var of Var.t
      datatype t =  Eq of expr * expr

      fun toString bp = case bp of
          Eq (Int i1,Int i2) => (Int.toString i1) ^ " = " 
            ^ (Int.toString i2)
        | Eq (Bool b1,Bool b2) => (Bool.toString b1) ^ " = " 
            ^ (Bool.toString b2)
        | Eq (Var v1, Var v2) => (Var.toString v1) ^ " = " 
            ^ (Var.toString v2)

      fun varEq (v1,v2) = Eq (Var v1,Var v2)

      fun applySubst subst t = 
      let
        val varSubst = varSubst subst
      in
        case t of
            Eq (Var v1, Var v2) => Eq (Var $ varSubst v1, Var $ varSubst v2)
          | Eq (Var v, e) => Eq (Var $ varSubst v, e)
          | Eq (e, Var v) => Eq (e, Var $ varSubst v)
      end
    end
    structure RelPredicate =
    struct
      type expr = RelLang.expr
      datatype t = Eq of expr * expr
                 | Sub of expr * expr
                 | SubEq of expr * expr
                 
      fun toString rp = case rp of
          Eq (e1,e2) => (RelLang.exprToString e1) ^ " = "
            ^ (RelLang.exprToString e2)
        | Sub (e1,e2) => (RelLang.exprToString e1) ^ " C "
            ^ (RelLang.exprToString e2)
        | SubEq (e1,e2) => (RelLang.exprToString e1) ^ " C= "
            ^ (RelLang.exprToString e2)

      fun exprMap rp (f : RelLang.expr -> RelLang.expr) = case rp of
          Eq (e1,e2) => Eq (f e1, f e2)
        | Sub (e1,e2) => Sub (f e1, f e2)
        | SubEq (e1,e2) => SubEq (f e1, f e2)

      fun applySubst subst t = exprMap t 
        (RelLang.applySubsts $ Vector.new1 subst)
      
      fun mapRExpr t f = 
        let
          val doIt = fn (e1,e2) => (f e1, f e2)
        in
          case t of Eq x => Eq $ doIt x
          | Sub x => Sub $ doIt x
          | SubEq x => SubEq $ doIt x
        end 
    end
    datatype t =  True
               |  Base of BasePredicate.t 
               |  Rel of RelPredicate.t
               |  Exists of TyDBinds.t * t
               |  Conj of t * t
               |  Disj of t * t

    fun layout t = case t of
        True => L.str "true" 
      | Base bp => L.str $ BasePredicate.toString bp
      | Rel rp => L.str $ RelPredicate.toString rp 
      | Exists (binds,t) => Pretty.nest ("exist",(TyDBinds.layout binds),
          layout t)
      | Conj (e1,e2) => L.align $ L.separateLeft ([(layout e1), (
          layout e2)],"/\\ ")
      | Disj (e1,e2) => L.align $ L.separateLeft ([(layout e1), (
          layout e2)],"\\/ ")

    fun truee _ = True

    fun conj (t1,t2) = Conj (t1,t2)

    fun conjR (t,r) = Conj (t,Rel r)

    fun conjP (t,p) = Conj (t,Base p)

    fun applySubst (subst as (new,old)) t = case t of
        True => True
      | Base bp => Base (BasePredicate.applySubst subst bp)
      | Rel rp => Rel (RelPredicate.applySubst subst rp)
      | Exists (tyDB,t) => if (TyDBinds.mem tyDB old)
            then Error.bug "Attempted substitution on existentially \
              \ quantified variable"
            else Exists (tyDB,applySubst subst t)
      | Conj (t1,t2) => Conj (applySubst subst t1, applySubst subst t2)
      | Disj (t1,t2) => Disj (applySubst subst t1, applySubst subst t2)

    (* telescoped substitutions *)
    fun applySubsts substs t = Vector.foldr (substs, t, fn (subst,t) =>
      applySubst subst t)

    fun exists (tyb,t) = Exists (tyb,t)

    fun disj (t1,t2) = Disj (t1,t2)

    fun mapRExpr t f = case t of
        Rel rp => Rel $ RelPredicate.mapRExpr rp f
      | Exists (binds,t) => Exists (binds, mapRExpr t f)
      | Conj (t1,t2) => Conj (mapRExpr t1 f, mapRExpr t2 f)
      | Disj (t1,t2) => Disj (mapRExpr t1 f, mapRExpr t2 f)
      | _ => t
  end

  structure RefinementType =
  struct
    datatype t = Base of Var.t * TypeDesc.t * Predicate.t
          | Tuple of (Var.t * t) vector
          | Arrow of (Var.t * t) * t
          (* Records are tuples with fixed bound var *)
          (* Needs extension for {'a | r} list *)

    val symbase = "v_"

    val count = ref 0

    val genVar = fn _ => 
      let val id = symbase ^ (Int.toString (!count))
          val _ = count := !count + 1
      in
        Var.fromString id 
      end

    fun fromTyD tyD =
      let
        open TypeDesc
      in
        case tyD of
          Tarrow (td1,td2) => Arrow ((genVar (), fromTyD td1),
            fromTyD td2)
        | Trecord tdrec => Tuple (Vector.map (Record.toVector tdrec, 
            fn (lbl,td) => (Var.fromString $ Field.toString lbl, 
              fromTyD td)))
        | tyD => Base (genVar(), tyD, Predicate.truee())
      end

    
    fun layout rty = case rty of
          Base(var,td,pred) => L.seq [L.str ("{" ^ (Var.toString var) 
            ^ ":" ^ (TypeDesc.toString td) ^ " | "), 
            Predicate.layout pred, L.str "}"]
        | Tuple tv => L.vector $ Vector.map (tv, fn (v,t) => 
            L.seq [L.str $ Var.toString v, L.str ":", layout t])
        | Arrow ((v1,t1),t2) => L.align $ L.separateLeft (
            [L.seq [L.str $ Var.toString v1, L.str ":", layout t1], 
            layout t2]," -> ")

    fun mapBaseTy t f = case t of
        Base (v,t,p) => Base $ f (v,t,p)
      | Tuple tv => Tuple $ Vector.map (tv,fn (v,t) => 
          (v,mapBaseTy t f))
      | Arrow ((v1,t1),t2) => Arrow ((v1,mapBaseTy t1 f), 
          mapBaseTy t2 f)

    fun mapTyD t f = mapBaseTy t (fn (v,t,p) => (v,f t,p)) 
      
    fun applySubsts substs refty = 
      mapBaseTy refty (fn (bv,t,pred) =>
        if Vector.exists (substs,fn(n,ol) => varStrEq (ol,bv))
          then Error.bug "Attempted substitution of bound var"
          else (bv,t,Predicate.applySubsts substs pred))

    fun alphaRenameToVar refty newbv = case refty of
        Base (bv,t,p) => Base (newbv,t,
          Predicate.applySubst (newbv,bv) p)
      | _ => Error.bug "alphaRename attempted on non-base type"

    fun alphaRename refty = alphaRenameToVar refty (genVar())
   
    val instTyvars = fn (eqs,t) => instTyvars (eqs,t,mapTyD)

    fun mapRExpr (t:t) (f : RelLang.expr -> RelLang.expr) = case t of
        Base (v,t,p) => Base (v, t, Predicate.mapRExpr p f)
      | Tuple tv => Tuple $ Vector.map (tv, fn (v,t) => 
          (v, mapRExpr t f))
      | Arrow ((v,t1), t2) => Arrow ((v, mapRExpr t1 f), mapRExpr t2 f)

  end

  structure RefinementSortScheme =
  struct
    open RelLang
    type paramrefty = {params : (RelVar.t * 
                   (* abstract relations are simple projections *)
                              SimpleProjSort.t) vector,
                            refty : RefinementType.t }
    datatype t = T of {reltyvars :RelTyvar.t vector,
                       constraints : RelTyConstraint.t vector,
                        paramrefty : paramrefty }
    fun paramRefTy (params,refty) = {params = params, refty = refty}

    fun prtMapTyD {params, refty} f = {params = Vector.map 
          (params, fn (r,spt) =>
            (r,SimpleProjSort.mapTyD spt f)),
         refty = RefinementType.mapTyD refty f}

    fun mapTyD (T {reltyvars, constraints, paramrefty}) f =
      T {reltyvars = reltyvars, 
          constraints = Vector.map (constraints, fn c =>
            RelTyC.mapTyD c f),
          paramrefty = prtMapTyD paramrefty f}

    fun prtInstRelTyvars (eqs, {params, refty}) =
      let
        val ps' = Vector.map (params, fn (v,sps) => 
          (v, SPS.instRelTyvars (eqs,sps)))
        (* refty is agnostic of reltys *)
        val refty' = refty
      in
        {params = ps', refty = refty'}
      end

    fun instantiate (eqs, T {reltyvars, constraints, paramrefty}) =
      let
        val _ = assert (isValidInst (eqs, reltyvars, RelTyvar.eq),
           "RelVar instantiation incomplete")
        val cs' = RelTyC.instRelTyvars (eqs,constraints)
        val prt' = prtInstRelTyvars (eqs, paramrefty)
      in
        (cs',prt')
      end

    val instTyvars = fn (eqs, t) => instTyvars (eqs, t, mapTyD)

    fun instRelParams (eqs, {params,refty}) =
      let
        val _ = assert (isValidInst (eqs, Vector.map (params,fst), 
          RelVar.eq), "Relvar instantiation incomplete")
      in
        RefinementType.mapRExpr refty (fn re => RelLang.instRelVars
          (eqs,re))
      end

    fun generalizeWith (reltyvs, cs, prefty) =
      T {reltyvars = reltyvs, constraints = cs, paramrefty = prefty}

    fun fromRefTy refty = 
      let
        val empty = fn _ => Vector.new0 ()
      in
        generalizeWith (empty(), empty(), paramRefTy (empty(), refty))
      end 

    fun toRefTy (T {paramrefty = {refty,...}, ...}) = refty

    fun prtLayout {params,refty} = 
      let
        fun typedParamLyt (r,sprojty) = L.str $ 
          (RelVar.toString r) ^ " :: " ^
          (SimpleProjSort.toString sprojty)
        val paramslyt = L.vector $ Vector.map (params, typedParamLyt)
        val reftylyt = RefinementType.layout refty
      in
        L.seq [paramslyt, L.str ". ", reftylyt]
      end

    fun layout (T {reltyvars, constraints, paramrefty}) = 
      let
        val rtyvlyt = L.vector $ Vector.map (reltyvars,fn rtyv =>
          L.str $ RelTyvar.toString rtyv)
        val prflyt = prtLayout paramrefty
      in
        L.seq [rtyvlyt, L.str ". ", prflyt]
      end

  end

  structure RefinementTypeScheme =
  struct
    datatype t = T of {tyvars : Tyvar.t vector,
                 sortscheme : RefinementSortScheme.t}
  
    val generalize = fn (tyvars, sortscheme) =>
      T {tyvars = tyvars, sortscheme = sortscheme}

    fun fromRefTy refty = T {tyvars = Vector.new0 (),
      sortscheme = RefinementSortScheme.fromRefTy refty}

    val specialize = fn (T {tyvars,sortscheme}) =>
      sortscheme

    val tyvars = fn (T {tyvars,sortscheme}) =>
      tyvars 

    fun toRefTy (T{sortscheme, ...}) = 
      RefinementSortScheme.toRefTy sortscheme
    
    fun layout (T {tyvars,sortscheme}) =
      let
        val tyvlyt = L.vector $ Vector.map (tyvars,fn tyv =>
          L.str $ Tyvar.toString tyv)
        val sslyt = RefinementSortScheme.layout sortscheme
      in
        L.seq [tyvlyt, L.str ". ", sslyt]
      end

    fun instantiate (T{tyvars,sortscheme},tydvec) =
      let
        val len = Vector.length
        val _ = assert (len tyvars = len tydvec,
          "insufficient number of type args")
        val substs = Vector.zip (tydvec,tyvars)
        (*
         * It is possible that we encounter a tyvar
         * that is not generalized in this RefTyS.
         * We do not panic.
         *)
      in
        RefinementSortScheme.mapTyD sortscheme
          (TypeDesc.instantiateTyvars substs)
      end
  end

  structure RelSpec =
  struct
    structure TypeSpec =
    struct
      datatype t = T of {name:Var.t,
                         params: RelVar.t vector,
                         refty : RefinementType.t}
      val layout = fn T {name=var,params,refty} => L.seq [
        L.str ((Var.toString var) ^ " : "),
        L.str (Vector.toString RelVar.toString params),
        RefinementType.layout refty]
    end
    datatype t = T of {reldecs : StructuralRelation.t vector,
                       typespecs : TypeSpec.t vector}
    val layout = fn T ({reldecs,typespecs,...}) =>
      let 
        val srs = Vector.toString StructuralRelation.toString reldecs
        val tslyt = L.align $ Vector.toListMap (typespecs,
          TypeSpec.layout)
      in
        L.align [L.str srs,tslyt]
      end
  end 
end

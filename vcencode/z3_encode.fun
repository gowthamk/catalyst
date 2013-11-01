functor Z3_Ctx (S : Z3_CTX_STRUCTS) : Z3_CTX = 
struct
  open S
  fun mkDefaultContext () =
    let
      val cfg = Z3_mk_config ()
      val ctx = Z3_mk_context cfg
      val _   = Z3_del_config cfg
    in
      ctx
    end

  val checkContext = Z3_check
  val delContext = Z3_del_context
end

functor Z3_Encode (S : Z3_ENCODE_STRUCTS) :> Z3_ENCODE =
struct
  open S
  exception InvalidOperation
  datatype sort = Int of z3_sort
                | Bool of z3_sort
                | T of string * z3_sort
  datatype ast = AST of z3_ast * sort
  (*
   * Set Invariant : len(ty) = 1 + len(domain(pred)) 
   *)
  datatype set = Set of { ty : sort vector,
                         pred : ast vector -> z3_ast}
  datatype struc_rel = SR of {rel : ast -> set}
  type assertion = z3_ast

  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  val ctx = freshContext
  fun mkGenName (cnt,base) =
    let
      val count = ref cnt
    in
      fn () => 
        let 
          val name = base ^ (Int.toString (!count))
          val _ = count := !count + 1
        in
          name
        end
    end
  val genTypeName = mkGenName (0,"T")
  val genSetName = mkGenName (0,"set")


  val mkSym = fn name => Z3_mk_string_symbol (ctx,name)

  (*
   * bool and int sorts declared here are not exposed.
   * Ones that are exposed are declared at the end.
   *)
  val bool_sort = Z3_mk_bool_sort ctx

  val int_sort = Z3_mk_int_sort ctx

  val flse = Z3_mk_false ctx

  fun mkUninterpretedSort () = (fn name => T (name, Z3_mk_uninterpreted_sort 
    (ctx, mkSym name))) (genTypeName ())

  fun typeCheckAst (AST (ast,sort),sort') = case (sort,sort') of
      (Int _,Int _) => true
    | (Bool _ , Bool _) => true
    | (T (name1,_), T (name2, _)) => name1 = name2
    | _ => false

  fun astToZ3Ast (AST (z3_ast,sort)) = z3_ast

  fun astToString (AST (z3_ast,_)) =
    Z3_ast_to_string (ctx,z3_ast)

  fun sortOfAst (AST (_,sort)) = sort

  fun sortToZ3Sort sort = case sort of Int t => t 
    | Bool t => t | T (name,t) => t

  fun mkEq (AST (x1,_),AST (x2,_)) = Z3_mk_eq (ctx,x1,x2)

  fun mkConst (name,sort) =
    AST (Z3_mk_const (ctx, mkSym name, sortToZ3Sort sort),sort)

  fun mkBoundVar ctx (index,sort) = 
    AST (Z3_mk_bound (ctx,index,sortToZ3Sort sort),sort)

  fun mkSet (name,sorts) =
    let
      val nargs = Vector.length sorts
      val z3_sorts = Vector.map (sorts, sortToZ3Sort)
      val func = Z3_mk_func_decl (ctx, mkSym name, nargs,
        z3_sorts, bool_sort)
      val pred = fn asts => 
        let
          val _ = assert (Vector.forall2 (asts,sorts,typeCheckAst),
            "Arguments of wrong type to set "^name)
          val z3_asts = Vector.map (asts,astToZ3Ast)
        in
          Z3_mk_app (ctx, func, nargs, z3_asts)
        end
    in
      Set {ty = sorts, pred = pred}
    end

  fun mkStrucRel (name,sorts) =
    let
      val nargs = Vector.length sorts
      val domainTy = Vector.sub (sorts,0)
      val rel = fn ast => 
        let
          val _ = assert (typeCheckAst (ast,domainTy),
            "Type error at app of relation "^name)
          val setName = name ^ (astToString ast)
          val Set {ty,pred} = mkSet (setName,sorts)
          val pred' = fn asts => pred $ Vector.concat 
            [Vector.new1 ast, asts]
        in
          Set {ty = ty, pred = pred'}
        end
    in
      SR {rel = rel}
    end

  fun applyStrucRel (SR {rel}, ast) = rel ast

  fun mkSetProp (sorts : sort vector, propfn : ast vector -> 
    (z3_pattern vector * z3_ast)) =
    let
      val numbvs = Vector.length sorts
      val bvs = Vector.mapi (sorts, fn (i,sort) => 
        mkBoundVar ctx (i,sort))
      val bvtys = Vector.map (sorts,sortToZ3Sort)
      (* 
       * De-brujin. Therefore: bv_n,bv_n-1,...,bv_0 
       *)
      val bvnames = Vector.tabulate (numbvs, fn i => mkSym 
        ("bv"^(Int.toString (numbvs-i-1))))
      val (patterns,prop) = propfn bvs
      val forall = Z3_mk_forall (ctx, 0, 
                    Vector.length patterns, 
                    patterns,
                    numbvs,
                    bvtys, 
                    bvnames, 
                    prop)
    in
      forall
    end

  fun dischargeAssertion asr = Z3_assert_cnstr (ctx,asr)

  fun assertSetProp (sorts,prop) =
    dischargeAssertion $ mkSetProp (sorts,prop)

  fun mkEmptySet (name,sorts) = 
    let
      val set as Set {ty,pred} = mkSet (name,sorts)
      val _ = assertSetProp (sorts, fn bvAsts =>
        let
          val fnapp = pred bvAsts
          val prop = Z3_mk_eq (ctx,fnapp,flse)
          val pattern = Z3_mk_pattern (ctx,1,Vector.new1 fnapp)
        in
          (Vector.new1 pattern, prop)
        end)
    in
      set end

  fun mkSingletonSet (name,asts) = 
    let
      val sorts = Vector.map (asts,sortOfAst)
      val set as Set {ty,pred} = mkSet (name,sorts)
      val _ = assertSetProp (sorts, fn bvAsts =>
        let
          val fnapp = pred bvAsts
          val eqs = Vector.map2 (bvAsts,asts,mkEq)
          val conj = Z3_mk_and (ctx,Vector.length eqs,eqs)
          val iff = Z3_mk_iff (ctx,fnapp,conj)
          val pattern = Z3_mk_pattern (ctx,1,Vector.new1 fnapp)
        in
          (Vector.new1 pattern, iff)
        end)
    in
      set
    end

  fun mkMultiPatterns multipatlist = Vector.fromList $ List.map 
    (multipatlist, fn terms => Z3_mk_pattern (ctx, List.length terms,
      Vector.fromList terms))

  fun mkSimplePatterns patlist = Vector.fromList $ List.map (patlist,
    fn pat => Z3_mk_pattern (ctx, 1, Vector.fromList [pat]))

  fun mkSetEqAssertion (Set {ty=sorts1,pred=pred1}, 
    Set {ty=sorts2,pred=pred2}) = 
    (*
     * Pre-condition of sorts1 = sorts2 is automatically
     * checked when pred1 and pred2 are applied
     *)
    mkSetProp (sorts1, fn bvAsts => 
      let
        val fnapp1 = pred1 bvAsts
        val fnapp2 = pred2 bvAsts
        val iff = Z3_mk_iff (ctx,fnapp1,fnapp2)
        val pats = mkSimplePatterns [fnapp1,fnapp2]
      in
        (pats,iff)
      end)
   
  fun mkUnion (Set {ty=sorts1,pred=pred1},
    Set {ty=sorts2,pred=pred2}) =
    let
      val s as Set {ty,pred} = mkSet (genSetName (), sorts1)
      val _ = assertSetProp (ty, fn bvAsts =>
        let
          val fnapp = pred bvAsts
          val fnapp1 = pred1 bvAsts
          val fnapp2 = pred2 bvAsts
          val disj = Z3_mk_or (ctx,2,Vector.fromList [fnapp1,fnapp2])
          val iff = Z3_mk_iff (ctx,fnapp,disj)
          val pats = mkSimplePatterns [fnapp, fnapp1, fnapp2]
        in
          (pats, iff)
        end)
    in
      s
    end
   
  fun mkCrossPrd (Set {ty=sorts1,pred=pred1},
    Set {ty=sorts2,pred=pred2}) =
    let
      val sorts = Vector.concat [sorts1,sorts2]
      val s as Set {ty,pred} = mkSet (genSetName (), sorts1)
      val _ = assertSetProp (ty, fn bvAsts =>
        let
          val bvAsts1 = Vector.prefix (bvAsts,Vector.length sorts1)
          val bvAsts2 = Vector.dropPrefix (bvAsts, 
            Vector.length sorts1)
          val fnapp = pred bvAsts
          val fnapp1 = pred1 bvAsts1
          val fnapp2 = pred2 bvAsts2
          val conj = Z3_mk_and (ctx,2,Vector.fromList [fnapp1,fnapp2])
          val iff = Z3_mk_iff (ctx,fnapp,conj)
          val pats = mkMultiPatterns [[fnapp], [fnapp1,fnapp2]]
        in
          (pats, iff)
        end)
    in
      s
    end

  fun mkNot asr = Z3_mk_not (ctx,asr) 

  fun mkConstEqAssertion (ast1 as AST (x1,s1), AST (x2,s2)) = 
    (typeCheckAst (ast1,s2); Z3_mk_eq (ctx,x1,x2))

  val bool_sort = Bool $ Z3_mk_bool_sort ctx

  val int_sort = Int $ Z3_mk_int_sort ctx

end

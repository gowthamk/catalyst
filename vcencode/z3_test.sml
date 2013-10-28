open Z3_FFI;

fun $ (f,arg) = f arg
infixr 5 $

fun simple_example () = 
  let
    val cfg = Z3_mk_config ()
    val ctx = Z3_mk_context cfg
    val str = Z3_context_to_string ctx
  in
    print str;
    print "done"
  end;

fun demorgan () =
  let
    val cfg                = Z3_mk_config ()
    val ctx                = Z3_mk_context cfg
    val _                  = Z3_del_config cfg
    val bool_sort          = Z3_mk_bool_sort ctx
    val symbol_x           = Z3_mk_int_symbol (ctx, 0)
    val symbol_y           = Z3_mk_int_symbol (ctx, 1)
    val x                  = Z3_mk_const(ctx, symbol_x, bool_sort)
    val y                  = Z3_mk_const(ctx, symbol_y, bool_sort)
    (* De Morgan - with a negation around *)
    (* !(!(x && y) <-> (!x || !y)) *)
    val not_x              = Z3_mk_not(ctx, x)
    val not_y              = Z3_mk_not(ctx, y)
    val args               = Array.fromList [x,y]
    val x_and_y            = Z3_mk_and(ctx, 2, args)
    val ls                 = Z3_mk_not(ctx, x_and_y)
    val args               = Array.fromList [not_x,not_y]
    val rs                 = Z3_mk_or(ctx, 2, args)
    val conjecture         = Z3_mk_iff(ctx, ls, rs)
    val negated_conjecture = Z3_mk_not(ctx, conjecture)
    val _ = Z3_assert_cnstr(ctx, negated_conjecture)
    val res = Z3_check ctx
    val _ = Z3_del_context ctx
  in
    case res of
      ~1 => print "DeMorgan is valid\n"
    | 0 => print "Undef\n"
    | 1 => print "Demorgan is invalid"
    | _ => raise (Fail "z3_lbool expected. Got something else.")
  end;

fun declareSet ctx (name,elem_sort) =
  let
    val sym = Z3_mk_string_symbol (ctx,name)
    val bool_sort = Z3_mk_bool_sort ctx
  in
      Z3_mk_func_decl (ctx, sym, 1,
        Array.fromList [elem_sort], bool_sort)
  end

fun declareSets ctx (names,ty) = 
  List.map (fn (name) => declareSet ctx (name,ty)) names

fun assertSingleton (ctx, set, el : z3_ast, elty) =
  let
    val bv_sym = Z3_mk_string_symbol (ctx, "bv")
    val bvs = Array.fromList [bv_sym]
    val bvtys = Array.fromList [elty]
    val bv_ast = Z3_mk_const (ctx, bv_sym, elty) 
    val args = Array.fromList [bv_ast]
    val fnapp = Z3_mk_app (ctx, set, 1, args)
    val eq = Z3_mk_eq (ctx,bv_ast,el)
    val iff = Z3_mk_iff (ctx,eq,fnapp)
    val pattern = Z3_mk_pattern (ctx,1,Array.fromList [fnapp])
    val _ = print $ Z3_pattern_to_string (ctx, pattern)
    val forall = Z3_mk_forall (ctx, 0, 1, Array.fromList [pattern], 1, 
       bvtys, bvs, iff)
    val _ = print "Hello\n"
  in
    Z3_assert_cnstr (ctx, forall)
  end

fun assertUnion (ctx, set, set1, set2, elty) = 
  let
    val bv_sym = Z3_mk_string_symbol (ctx, "bv")
    val bvs = Array.fromList [bv_sym]
    val bvtys = Array.fromList [elty]
    val bv_ast = Z3_mk_const (ctx, bv_sym, elty) 
    val args = Array.fromList [bv_ast]
    val fnapp = Z3_mk_app (ctx, set, 1, args)
    val fnapp1 = Z3_mk_app (ctx, set1, 1, args)
    val fnapp2 = Z3_mk_app (ctx, set2, 1, args)
    val disj = Z3_mk_or (ctx,2,Array.fromList [fnapp1,fnapp2])
    val iff = Z3_mk_iff (ctx,fnapp,disj)
    val mk_pat = fn ast => Z3_mk_pattern (ctx,1,Array.fromList [ast])
    val pats = Array.fromList $ List.map mk_pat [fnapp,fnapp1,fnapp2]
    val _  = Array.app (fn pat => print $ Z3_pattern_to_string 
      (ctx, pat)) pats
    val forall = Z3_mk_forall (ctx, 0, 3, pats, 1, 
       bvtys , bvs, iff)
  in
    Z3_assert_cnstr (ctx, forall)
  end


fun assertSetEq (ctx, set1, set2, elty) = 
  let
    val bv_sym = Z3_mk_string_symbol (ctx, "bv")
    val bvs = Array.fromList [bv_sym]
    val bvtys = Array.fromList [elty]
    val bv_ast = Z3_mk_const (ctx, bv_sym, elty) 
    val args = Array.fromList [bv_ast]
    val fnapp1 = Z3_mk_app (ctx, set1, 1, args)
    val fnapp2 = Z3_mk_app (ctx, set2, 1, args)
    val iff = Z3_mk_iff (ctx,fnapp1,fnapp2)
    val mk_pat = fn ast => Z3_mk_pattern (ctx,1,Array.fromList [ast])
    val pats = Array.fromList $ List.map mk_pat [fnapp1,fnapp2]
    val forall = Z3_mk_forall (ctx, 0, 2, pats, 1, 
       bvtys , bvs, iff)
  in
    Z3_assert_cnstr (ctx, forall)
  end

fun assertSetNotEq (ctx, set1, set2, elty) = 
  let
    val bv_sym = Z3_mk_string_symbol (ctx, "bv")
    val bvs = Array.fromList [bv_sym]
    val bvtys = Array.fromList [elty]
    val bv_ast = Z3_mk_const (ctx, bv_sym, elty) 
    val args = Array.fromList [bv_ast]
    val fnapp1 = Z3_mk_app (ctx, set1, 1, args)
    val fnapp2 = Z3_mk_app (ctx, set2, 1, args)
    val iff = Z3_mk_iff (ctx,fnapp1,fnapp2)
    val mk_pat = fn ast => Z3_mk_pattern (ctx,1,Array.fromList [ast])
    val pats = Array.fromList $ List.map mk_pat [fnapp1,fnapp2]
    val forall = Z3_mk_forall (ctx, 0, 2, pats, 1, 
       bvtys , bvs, iff)
    val negated = Z3_mk_not (ctx,forall)
  in
    Z3_assert_cnstr (ctx, negated)
  end

fun reverse_proof () =
  let
    val cfg                = Z3_mk_config ()
    val ctx                = Z3_mk_context cfg
    val _                  = Z3_del_config cfg
    (* functions in this context *)
    val declareSet         = declareSet ctx
    val declareSets        = declareSets ctx
    val mkSym              = fn str => Z3_mk_string_symbol (ctx,str)
    val bool_sort          = Z3_mk_bool_sort ctx
    val t_sym              = Z3_mk_string_symbol (ctx,"T")
    val t_sort             = Z3_mk_uninterpreted_sort (ctx,t_sym)
    val [rmemxs, rmeml,
        rmemxs1, rmemv,
        rmemx1]            = declareSets (["Rmemxs","Rmeml",
                              "Rmemxs1", "Rmemv","Rmemx1"],t_sort)
    (* Declare constants *)
    val x_sym              = Z3_mk_string_symbol (ctx,"x")
    val x1_sym             = Z3_mk_string_symbol (ctx,"x1")
    val x                  = Z3_mk_const (ctx,x_sym,t_sort)
    (* Declare tuple type *) 
    (*val pair_sym           = mkSym "Pair"
    val recog_sym          = mkSym "is_pair"
    val first_sym          = mkSym "first"
    val second_sym         = mkSym "second"
    val fld_names          = Array.fromList [first_sym,second_sym]
    val mk-pair_sym        = mkSym "mk-pair"
    val sorts              = Array.fromList [t_sort,t_sort]
    val sort_refs          = Array.fromList [0,0]
    val pair_cons          = Z3_mk_constructor (ctx, mk-pair_sym,
                              recog_sym,2,fld_names,sorts,sort_refs)
    val tuple_ty           = Z3_mk_datatype (ctx, pair_sym, 1, 
                              Array.fromList [pair_cons])
    val pair_fn            = Z3_mk_func_decl (ctx, mkSym "pair_decl",
                              2, sorts, tuple_ty)
    val is_pair_fn         = Z3_mk_func_decl (ctx, mkSym "is_pair_decl",
                              1, Array.fromList [tuple_ty], bool_sort)
    val first_fn           = Z3_mk_func_decl (ctx, mkSym "first_decl",
                              1, Array.fromList [tuple_ty], t_sort)
    val second_fn          = Z3_mk_func_decl (ctx, mkSym "second_decl",
                              1, Array.fromList [tuple_ty], t_sort)
    val _                  = Z3_query_constructor (ctx, pair_cons, 2, 
                              ref pair_fn, ref is_pair_fn, Array.fromList 
                              [first_fn,second_fn])
    val [robsxs, robsl,
         roasxs1, roasx1,
         roasv]            = declareSets (["Robsxs", "Robsl", "Roasxs1", 
                                "Roasx1", "Roasv"],t_sort)*)
    val setx               = declareSet ("setx",t_sort)
    val _                  = assertSingleton (ctx,setx,x,t_sort)
    val _                  = assertUnion (ctx,rmeml,setx,rmemxs,t_sort)
    val _                  = assertSetEq (ctx,rmemxs,rmemxs1,t_sort)
    val rmemx1             = declareSet ("rmemx1",t_sort)
    val _                  = assertSingleton (ctx,rmemx1,x,t_sort)
    val _                  = assertUnion (ctx,rmemv,rmemxs1,rmemx1,t_sort)
    val _                  = assertSetNotEq (ctx,rmemv,rmeml,t_sort)
    val res                = Z3_check ctx
    val _                  = Z3_del_context ctx
  in
    case res of
      ~1 => print "Reverse is typechecked\n"
    | 0 => print "Undef\n"
    | 1 => print "Reverse typechecking failed\n"
    | _ => raise (Fail "z3_lbool expected. Got something else.")
  end;

fun main () = 
  let 
    val _ = Z3_open_log ("z3.log")
    (*val _ = simple_example ()*)
    val _ = demorgan ()
    val _ = reverse_proof ()
    val _ = Z3_close_log ()
  in
    ()
  end;

main ();

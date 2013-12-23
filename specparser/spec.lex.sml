(*#line 14.10 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)functor SpecLexFun (structure Tokens : Spec_TOKENS)(*#line 1.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)structure Tokens = Tokens
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type svalue = Tokens.svalue
type lexresult = (svalue,pos) token
val line = ref 1
val debugFlag = ref false
val eof = fn () => Tokens.EOF(!line,!line)
val debug = fn s => if (!debugFlag) then print s else () 
(*
  Spec_TOKENS defined using term declaration in grm
*)
(*#line 18.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\000\000\000\000\000\000\000\000\000\052\054\000\000\053\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\052\000\000\000\000\000\000\000\051\050\049\048\047\045\044\042\
\\041\041\041\041\041\041\041\041\041\041\040\039\036\035\000\000\
\\000\006\006\033\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\032\006\006\031\006\006\030\000\029\000\006\
\\000\006\006\006\006\006\024\006\006\006\006\006\006\006\006\006\
\\006\006\016\006\012\006\009\006\006\006\006\005\004\003\000\000\
\\000"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (8, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\010\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\011\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\013\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\014\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\015\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\017\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (17, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\018\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\019\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\020\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (20, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\021\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\022\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\023\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (24, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\025\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (25, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\026\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (26, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\027\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (27, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\028\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\
\\007\007\007\007\007\007\007\007\007\007\000\000\000\034\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (36, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\037\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (37, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\038\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (41, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\041\041\041\041\041\041\041\041\041\041\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (42, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\043\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (45, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (52, 
"\000\000\000\000\000\000\000\000\000\052\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\052\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (53, 
"\000\000\000\000\000\000\000\000\000\000\054\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 71)], trans = 0},
{fin = [(N 80)], trans = 0},
{fin = [(N 69)], trans = 0},
{fin = [(N 87)], trans = 6},
{fin = [(N 87)], trans = 7},
{fin = [(N 87)], trans = 8},
{fin = [(N 87)], trans = 9},
{fin = [(N 87)], trans = 10},
{fin = [(N 20),(N 87)], trans = 6},
{fin = [(N 87)], trans = 12},
{fin = [(N 87)], trans = 13},
{fin = [(N 87)], trans = 14},
{fin = [(N 25),(N 87)], trans = 6},
{fin = [(N 87)], trans = 16},
{fin = [(N 87)], trans = 17},
{fin = [(N 87)], trans = 18},
{fin = [(N 87)], trans = 19},
{fin = [(N 87)], trans = 20},
{fin = [(N 87)], trans = 21},
{fin = [(N 87)], trans = 22},
{fin = [(N 16),(N 87)], trans = 6},
{fin = [(N 87)], trans = 24},
{fin = [(N 87)], trans = 25},
{fin = [(N 87)], trans = 26},
{fin = [(N 87)], trans = 27},
{fin = [(N 31),(N 87)], trans = 6},
{fin = [(N 75)], trans = 0},
{fin = [(N 73)], trans = 0},
{fin = [(N 39),(N 87)], trans = 6},
{fin = [(N 37),(N 87)], trans = 6},
{fin = [(N 44),(N 87)], trans = 33},
{fin = [(N 42)], trans = 0},
{fin = [(N 46)], trans = 0},
{fin = [], trans = 36},
{fin = [], trans = 37},
{fin = [(N 50)], trans = 0},
{fin = [(N 59)], trans = 0},
{fin = [(N 57)], trans = 0},
{fin = [(N 90)], trans = 41},
{fin = [], trans = 42},
{fin = [(N 53)], trans = 0},
{fin = [(N 55)], trans = 0},
{fin = [(N 35)], trans = 45},
{fin = [(N 78)], trans = 0},
{fin = [(N 61)], trans = 0},
{fin = [(N 33)], trans = 0},
{fin = [(N 63)], trans = 0},
{fin = [(N 67)], trans = 0},
{fin = [(N 65)], trans = 0},
{fin = [(N 7)], trans = 52},
{fin = [(N 4)], trans = 53},
{fin = [(N 4)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  16 => ((*#line 25.27 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "relation"; Tokens.RELATION(!line,yypos)(*#line 453.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 20 => ((*#line 26.22 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "var"; Tokens.VAR(!line,yypos)(*#line 455.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 25 => ((*#line 27.23 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "true"; Tokens.TRUE(!line,yypos)(*#line 457.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 31 => ((*#line 28.24 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "false"; Tokens.FALSE(!line,yypos)(*#line 459.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 33 => ((*#line 29.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "plus"; Tokens.PLUS(!line,yypos)(*#line 461.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 35 => ((*#line 30.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "minus"; Tokens.MINUS(!line,yypos)(*#line 463.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 37 => ((*#line 31.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "union"; Tokens.UNION(!line,yypos)(*#line 465.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 39 => ((*#line 32.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "crossprd"; Tokens.CROSSPRD(!line,yypos)(*#line 467.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 4 => ((*#line 23.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)line := (!line)+1; lex()(*#line 469.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 42 => ((*#line 33.21 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "subseteq"; Tokens.SUBSETEQ(!line,yypos)(*#line 471.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 44 => ((*#line 34.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "subset"; Tokens.SUBSET(!line,yypos)(*#line 473.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 46 => ((*#line 35.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "equalop";Tokens.EQUALOP(!line,yypos)(*#line 475.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 50 => ((*#line 36.22 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "iff";Tokens.IFF(!line,yypos)(*#line 477.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 53 => ((*#line 37.22 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "conj";Tokens.CONJ(!line,yypos)(*#line 479.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 55 => ((*#line 38.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "dot";Tokens.DOT(!line,yypos)(*#line 481.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 57 => ((*#line 39.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "colon\n";Tokens.COLON(!line,yypos)(*#line 483.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 59 => ((*#line 40.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "semicolon\n";Tokens.SEMICOLON(!line,yypos)(*#line 485.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 61 => ((*#line 41.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "comma\n";Tokens.COMMA(!line,yypos)(*#line 487.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 63 => ((*#line 42.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "star\n";Tokens.STAR(!line,yypos)(*#line 489.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 65 => ((*#line 43.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "lparen\n"; Tokens.LPAREN(!line,yypos)(*#line 491.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 67 => ((*#line 44.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "rparen\n"; Tokens.RPAREN(!line,yypos)(*#line 493.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 69 => ((*#line 45.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "lcurly\n"; Tokens.LCURLY(!line,yypos)(*#line 495.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 7 => ((*#line 24.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "whitespace"; lex()(*#line 497.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 71 => ((*#line 46.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "rcurly\n"; Tokens.RCURLY(!line,yypos)(*#line 499.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 73 => ((*#line 47.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "lbrace\n"; Tokens.LBRACE(!line,yypos)(*#line 501.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 75 => ((*#line 48.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "rbrace\n"; Tokens.RBRACE(!line,yypos)(*#line 503.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 78 => ((*#line 49.21 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "arrow\n"; Tokens.ARROW(!line,yypos)(*#line 505.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 80 => ((*#line 50.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug "pipe\n"; Tokens.PIPE(!line,yypos)(*#line 507.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
)
| 87 => let val yytext=yymktext() in (*#line 51.25 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug ("var: "^yytext^"\n"); Tokens.ID(yytext,!line,yypos)(*#line 509.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
 end
| 90 => let val yytext=yymktext() in (*#line 52.23 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex"*)debug ("int: "^yytext^"\n"); 
                      case Int.fromString yytext of
                          SOME n => Tokens.INT(n,!line,yypos) 
                        | NONE => raise (Fail "Number couldn't be obtained")
                     (*#line 515.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.lex.sml"*)
 end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end

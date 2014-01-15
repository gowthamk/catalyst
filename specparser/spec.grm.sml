functor SpecLrValsFun (structure Token : TOKEN
                                structure Spec : SPEC_LANG) : Spec_LRVALS = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "spec.grm"*)(*  User declarations section for helper functions *)
open Spec
open RelLang
structure TypeSpec = RelSpec.TypeSpec
structure RefTy = RefinementType
val defaultCons = Con.default
val symbase = "sp_"
val count = ref 0
val genVar = fn _ => 
  let val id = symbase ^ (Int.toString (!count))
      val _ = count := !count + 1
  in
    Var.fromString id 
  end
fun $ (f,arg) = f arg
infixr 5 $

(*#line 25.1 "spec.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\085\000\017\000\056\000\019\000\055\000\027\000\054\000\000\000\
\\001\000\002\000\102\000\003\000\101\000\018\000\100\000\027\000\099\000\
\\028\000\098\000\000\000\
\\001\000\002\000\102\000\003\000\101\000\027\000\099\000\028\000\098\000\000\000\
\\001\000\008\000\110\000\009\000\109\000\010\000\108\000\000\000\
\\001\000\010\000\035\000\017\000\034\000\019\000\033\000\027\000\032\000\000\000\
\\001\000\010\000\044\000\000\000\
\\001\000\010\000\105\000\017\000\034\000\019\000\033\000\027\000\032\000\000\000\
\\001\000\013\000\012\000\000\000\
\\001\000\013\000\064\000\000\000\
\\001\000\014\000\177\000\015\000\177\000\018\000\177\000\023\000\179\000\000\000\
\\001\000\014\000\178\000\015\000\178\000\018\000\178\000\023\000\180\000\000\000\
\\001\000\014\000\010\000\000\000\
\\001\000\014\000\011\000\000\000\
\\001\000\017\000\016\000\027\000\015\000\000\000\
\\001\000\017\000\025\000\019\000\024\000\027\000\023\000\000\000\
\\001\000\017\000\034\000\019\000\033\000\027\000\032\000\000\000\
\\001\000\017\000\056\000\019\000\055\000\027\000\054\000\000\000\
\\001\000\017\000\074\000\000\000\
\\001\000\017\000\078\000\000\000\
\\001\000\018\000\026\000\000\000\
\\001\000\018\000\063\000\000\000\
\\001\000\018\000\069\000\000\000\
\\001\000\018\000\071\000\000\000\
\\001\000\018\000\080\000\000\000\
\\001\000\018\000\091\000\000\000\
\\001\000\018\000\103\000\000\000\
\\001\000\018\000\113\000\000\000\
\\001\000\018\000\115\000\000\000\
\\001\000\020\000\061\000\024\000\060\000\000\000\
\\001\000\020\000\107\000\000\000\
\\001\000\020\000\117\000\000\000\
\\001\000\020\000\124\000\000\000\
\\001\000\022\000\114\000\000\000\
\\001\000\023\000\037\000\000\000\
\\001\000\027\000\014\000\000\000\
\\001\000\027\000\036\000\000\000\
\\001\000\027\000\038\000\000\000\
\\001\000\027\000\042\000\000\000\
\\001\000\027\000\047\000\000\000\
\\001\000\027\000\049\000\000\000\
\\001\000\027\000\058\000\000\000\
\\001\000\029\000\000\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\001\000\009\000\017\000\008\000\027\000\007\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\027\000\058\000\000\000\
\\137\000\000\000\
\\138\000\015\000\027\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\024\000\045\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\015\000\070\000\000\000\
\\148\000\015\000\070\000\017\000\073\000\019\000\033\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\000\000\
\\152\000\021\000\077\000\000\000\
\\153\000\000\000\
\\154\000\021\000\077\000\000\000\
\\155\000\000\000\
\\156\000\016\000\075\000\017\000\074\000\000\000\
\\156\000\017\000\074\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\006\000\089\000\007\000\088\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\027\000\093\000\000\000\
\\167\000\015\000\116\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\181\000\015\000\062\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\012\000\106\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\"
val actionRowNumbers =
"\046\000\011\000\012\000\043\000\
\\042\000\007\000\034\000\013\000\
\\046\000\046\000\014\000\019\000\
\\053\000\004\000\035\000\045\000\
\\044\000\009\000\033\000\092\000\
\\090\000\097\000\036\000\014\000\
\\037\000\034\000\060\000\005\000\
\\056\000\047\000\058\000\038\000\
\\039\000\016\000\040\000\014\000\
\\028\000\094\000\020\000\096\000\
\\008\000\054\000\016\000\015\000\
\\021\000\063\000\022\000\064\000\
\\074\000\066\000\072\000\049\000\
\\068\000\018\000\016\000\023\000\
\\051\000\093\000\000\000\098\000\
\\014\000\010\000\014\000\077\000\
\\057\000\017\000\055\000\062\000\
\\038\000\061\000\024\000\038\000\
\\083\000\067\000\069\000\016\000\
\\001\000\025\000\006\000\052\000\
\\101\000\029\000\003\000\100\000\
\\095\000\091\000\016\000\016\000\
\\065\000\059\000\026\000\082\000\
\\032\000\073\000\027\000\084\000\
\\086\000\089\000\030\000\088\000\
\\087\000\081\000\048\000\016\000\
\\000\000\099\000\016\000\016\000\
\\016\000\075\000\076\000\080\000\
\\070\000\031\000\002\000\078\000\
\\050\000\102\000\103\000\104\000\
\\105\000\071\000\079\000\085\000\
\\041\000"
val gotoT =
"\
\\001\000\124\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\023\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\011\000\000\000\
\\000\000\
\\003\000\015\000\004\000\002\000\023\000\001\000\000\000\
\\003\000\016\000\004\000\002\000\023\000\001\000\000\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\029\000\017\000\000\000\
\\000\000\
\\000\000\
\\007\000\029\000\008\000\028\000\009\000\027\000\010\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\024\000\039\000\025\000\019\000\026\000\018\000\027\000\038\000\
\\028\000\037\000\029\000\017\000\000\000\
\\000\000\
\\006\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\044\000\000\000\
\\012\000\046\000\000\000\
\\013\000\051\000\015\000\050\000\018\000\049\000\020\000\048\000\000\000\
\\005\000\055\000\000\000\
\\024\000\057\000\025\000\019\000\026\000\018\000\029\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\065\000\019\000\064\000\020\000\063\000\000\000\
\\007\000\066\000\008\000\028\000\009\000\027\000\010\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\070\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\074\000\000\000\
\\000\000\
\\015\000\065\000\019\000\077\000\020\000\063\000\000\000\
\\000\000\
\\005\000\079\000\000\000\
\\000\000\
\\015\000\065\000\019\000\082\000\020\000\063\000\030\000\081\000\
\\032\000\080\000\000\000\
\\000\000\
\\024\000\039\000\025\000\019\000\026\000\018\000\027\000\084\000\
\\028\000\037\000\029\000\017\000\000\000\
\\000\000\
\\024\000\085\000\025\000\019\000\026\000\018\000\029\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\088\000\000\000\
\\000\000\
\\000\000\
\\012\000\046\000\000\000\
\\016\000\090\000\000\000\
\\000\000\
\\000\000\
\\015\000\093\000\018\000\092\000\020\000\048\000\000\000\
\\021\000\095\000\022\000\094\000\000\000\
\\000\000\
\\007\000\102\000\008\000\028\000\009\000\027\000\010\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\065\000\019\000\109\000\020\000\063\000\000\000\
\\015\000\065\000\019\000\110\000\020\000\063\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\116\000\015\000\050\000\018\000\049\000\020\000\048\000\000\000\
\\015\000\065\000\019\000\082\000\020\000\063\000\030\000\117\000\
\\032\000\080\000\000\000\
\\000\000\
\\015\000\065\000\019\000\118\000\020\000\063\000\000\000\
\\015\000\065\000\019\000\119\000\020\000\063\000\000\000\
\\015\000\065\000\019\000\120\000\020\000\063\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\121\000\000\000\
\\000\000\
\\021\000\095\000\022\000\123\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 125
val numrules = 66
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | INT of  (int) | ID of  (string) | rpatom of  (Predicate.RelPredicate.t) | relpred of  (Predicate.RelPredicate.t) | pred of  (Predicate.t) | basety of  (RefinementType.t) | varty of  (Var.t*RefinementType.t) | vartyseq of  ( ( Var.t * RefinementType.t )  list) | vartyatom of  (Var.t*RefinementType.t) | reftyatom of  (RefinementType.t) | refty of  (RefinementType.t) | typespec of  (TypeSpec.t) | elemseq of  (elem list) | elem of  (elem) | ratom of  (expr) | rexpr of  (expr) | ieatom of  (ieatom) | ieatoms of  (ieatom list) | varop of  (Var.t option) | instexpr of  (instexpr) | rterm of  (term) | iterm of  (term) | idseq of  (Var.t list) | conargs of  (Var.t vector) | valpat of  (Pat.value) | pat of  (Pat.t) | patmatch of  (Pat.t*term) | patmatchseq of  ( ( Pat.t * term )  list) | paramseq of  (RelVar.t list) | params of  (RelVar.t list) | reldec of  (StructuralRelation.t) | decsandtys of  (RelSpec.t) | spec of  (RelSpec.t) | start of  (RelSpec.t)
end
type svalue = MlyValue.svalue
type result = RelSpec.t
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 28) => true | _ => false
val showTerminal =
fn (T 0) => "RELATION"
  | (T 1) => "TRUE"
  | (T 2) => "FALSE"
  | (T 3) => "PLUS"
  | (T 4) => "MINUS"
  | (T 5) => "UNION"
  | (T 6) => "CROSSPRD"
  | (T 7) => "SUBSETEQ"
  | (T 8) => "SUBSET"
  | (T 9) => "EQUALOP"
  | (T 10) => "IFF"
  | (T 11) => "CONJ"
  | (T 12) => "COLON"
  | (T 13) => "SEMICOLON"
  | (T 14) => "COMMA"
  | (T 15) => "STAR"
  | (T 16) => "LPAREN"
  | (T 17) => "RPAREN"
  | (T 18) => "LCURLY"
  | (T 19) => "RCURLY"
  | (T 20) => "LBRACE"
  | (T 21) => "RBRACE"
  | (T 22) => "ARROW"
  | (T 23) => "PIPE"
  | (T 24) => "DOT"
  | (T 25) => "VAR"
  | (T 26) => "ID"
  | (T 27) => "INT"
  | (T 28) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 28) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.spec spec, spec1left, spec1right)) :: rest671)) => let val  result = MlyValue.start ((*#line 101.15 "spec.grm"*)spec(*#line 430.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, spec1left, spec1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.decsandtys decsandtys, decsandtys1left, decsandtys1right)) :: rest671)) => let val  result = MlyValue.spec ((*#line 103.21 "spec.grm"*)decsandtys(*#line 434.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, decsandtys1left, decsandtys1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.decsandtys decsandtys, _, decsandtys1right)) :: _ :: ( _, ( MlyValue.reldec reldec, reldec1left, _)) :: rest671)) => let val  result = MlyValue.decsandtys ((*#line 106.18 "spec.grm"*)case decsandtys of RelSpec.T ({reldecs,typespecs}) => 
                    RelSpec.T {reldecs = Vector.fromList (reldec ::
                      (Vector.toList reldecs)), typespecs = typespecs}(*#line 438.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, reldec1left, decsandtys1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.decsandtys decsandtys, _, decsandtys1right)) :: _ :: ( _, ( MlyValue.typespec typespec, typespec1left, _)) :: rest671)) => let val  result = MlyValue.decsandtys ((*#line 110.18 "spec.grm"*)case decsandtys of RelSpec.T {reldecs,typespecs} => 
                    RelSpec.T {reldecs = reldecs, typespecs = 
                    Vector.fromList (typespec :: (Vector.toList typespecs))}(*#line 444.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, typespec1left, decsandtys1right), rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.decsandtys ((*#line 113.16 "spec.grm"*)RelSpec.T {reldecs = Vector.fromList [],
                  typespecs = Vector.fromList []}(*#line 450.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 117.12 "spec.grm"*)StructuralRelation.T {id=RelId.fromString ID,
                params = Vector.new0 (),
                map = Vector.fromList (List.map (patmatchseq,
                  fn (pat,rterm) => (SOME pat,rterm)))}(*#line 455.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, patmatchseq1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: _ :: ( _, ( MlyValue.params params, _, _)) :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 122.12 "spec.grm"*)StructuralRelation.T {id=RelId.fromString ID,
                params = Vector.fromList params,
                map = Vector.fromList (List.map (patmatchseq,
                  fn (pat,rterm) => (SOME pat,rterm)))}(*#line 462.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, patmatchseq1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.iterm iterm, _, iterm1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 127.12 "spec.grm"*)StructuralRelation.T{id=RelId.fromString ID,
                params = Vector.new0 (),
                map = Vector.fromList [(NONE,iterm)]}(*#line 469.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, iterm1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.iterm iterm, _, iterm1right)) :: _ :: _ :: ( _, ( MlyValue.params params, _, _)) :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 131.12 "spec.grm"*)StructuralRelation.T{id=RelId.fromString ID,
                params = Vector.fromList params,
                map = Vector.fromList [(NONE,iterm)]}(*#line 475.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, iterm1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.params ((*#line 135.14 "spec.grm"*)[RelVar.fromString ID](*#line 481.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.params params, _, params1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.params ((*#line 136.21 "spec.grm"*)(RelVar.fromString ID)::params(*#line 485.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, ID1left, params1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.paramseq ((*#line 138.16 "spec.grm"*)[RelVar.fromString ID](*#line 489.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.paramseq paramseq, _, paramseq1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.paramseq ((*#line 139.29 "spec.grm"*)(RelVar.fromString ID)::paramseq(*#line 493.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, ID1left, paramseq1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: _ :: ( _, ( MlyValue.patmatch patmatch, patmatch1left, _)) :: rest671)) => let val  result = MlyValue.patmatchseq ((*#line 141.42 "spec.grm"*)patmatch :: patmatchseq(*#line 497.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, patmatch1left, patmatchseq1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.patmatch patmatch, patmatch1left, patmatch1right)) :: rest671)) => let val  result = MlyValue.patmatchseq ((*#line 142.25 "spec.grm"*)[patmatch](*#line 501.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, patmatch1left, patmatch1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.pat pat, pat1left, _)) :: rest671)) => let val  result = MlyValue.patmatch ((*#line 144.31 "spec.grm"*)pat, Atom (Re rexpr)(*#line 505.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, pat1left, rexpr1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.pat ((*#line 151.11 "spec.grm"*)Pat.Con (Con.fromString ID, NONE)(*#line 509.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.valpat valpat, _, _)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.pat ((*#line 152.32 "spec.grm"*)Pat.Con (Con.fromString ID, SOME valpat)(*#line 513.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.valpat valpat, valpat1left, valpat1right)) :: rest671)) => let val  result = MlyValue.pat ((*#line 153.15 "spec.grm"*)Pat.Value valpat(*#line 517.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, valpat1left, valpat1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.idseq idseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.valpat ((*#line 155.31 "spec.grm"*)case idseq of 
              [x] => Pat.Var x
            | _ => Pat.Tuple (Vector.fromList idseq)(*#line 521.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.idseq idseq, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.valpat ((*#line 158.31 "spec.grm"*)Pat.Record (Record.fromVector
          (Vector.fromList (List.map (idseq, fn x => 
            (Field.Symbol (Field.Symbol.fromString (Var.toString x)), x)))) )(*#line 527.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, LCURLY1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.idseq ((*#line 162.13 "spec.grm"*)[Var.fromString ID](*#line 533.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.idseq idseq, _, idseq1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.idseq ((*#line 163.25 "spec.grm"*)(Var.fromString ID)::idseq(*#line 537.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, ID1left, idseq1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ieatom ieatom, ieatom1left, ieatom1right)) :: rest671)) => let val  result = MlyValue.iterm ((*#line 165.17 "spec.grm"*)Atom ieatom(*#line 541.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, ieatom1left, ieatom1right), rest671)
end
|  ( 24, ( ( _, ( _, _, STAR1right)) :: ( _, ( MlyValue.instexpr instexpr, instexpr1left, _)) :: rest671)) => let val  result = MlyValue.iterm ((*#line 166.24 "spec.grm"*)Star instexpr(*#line 545.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, instexpr1left, STAR1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.instexpr ((*#line 169.16 "spec.grm"*)Relation (RelId.fromString ID)(*#line 549.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, ID1left, ID1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ieatoms ieatoms, _, ieatoms1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.instexpr ((*#line 170.24 "spec.grm"*)Inst {args = 
                Vector.fromList ieatoms, rel = RelId.fromString ID}(*#line 553.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, ID1left, ieatoms1right), rest671)
end
|  ( 27, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ieatom ieatom, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.ieatoms ((*#line 173.33 "spec.grm"*)[ieatom](*#line 558.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ieatoms ieatoms, _, ieatoms1right)) :: _ :: ( _, ( MlyValue.ieatom ieatom, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.ieatoms ((*#line 174.41 "spec.grm"*)ieatom :: ieatoms(*#line 562.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, LBRACE1left, ieatoms1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.instexpr instexpr, instexpr1left, instexpr1right)) :: rest671)) => let val  result = MlyValue.ieatom ((*#line 176.20 "spec.grm"*)Ie instexpr(*#line 566.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, instexpr1left, instexpr1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ratom ratom, ratom1left, ratom1right)) :: rest671)) => let val  result = MlyValue.ieatom ((*#line 177.17 "spec.grm"*)Re ratom(*#line 570.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, ratom1left, ratom1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 179.31 "spec.grm"*)X(ratom,rexpr)(*#line 574.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 180.28 "spec.grm"*)U(ratom,rexpr)(*#line 578.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ratom ratom, ratom1left, ratom1right)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 181.16 "spec.grm"*)ratom(*#line 582.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, ratom1left, ratom1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RCURLY1right)) :: _ :: _ :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 183.38 "spec.grm"*)T (Vector.fromList [])(*#line 586.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 35, ( ( _, ( _, _, RCURLY1right)) :: _ :: ( _, ( MlyValue.elemseq elemseq, _, _)) :: _ :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 185.12 "spec.grm"*)T (Vector.fromList elemseq)(*#line 590.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 36, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.varop varop, _, _)) :: _ :: ( _, ( MlyValue.instexpr instexpr, instexpr1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 186.39 "spec.grm"*)case (instexpr,varop) of 
            (Relation rid, NONE) => R1 (RelVar.fromString 
                (RelId.toString rid))
          | (_,SOME v) => R2 (instexpr,v)
          | _ => raise (Fail "Missing arguments for an \
            \ instantiated relation\n")(*#line 594.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, instexpr1left, RPAREN1right), rest671)
end
|  ( 37, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.rexpr rexpr, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 192.30 "spec.grm"*)rexpr(*#line 603.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.varop ((*#line 194.13 "spec.grm"*)SOME (Var.fromString ID)(*#line 607.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, ID1left, ID1right), rest671)
end
|  ( 39, ( rest671)) => let val  result = MlyValue.varop ((*#line 195.13 "spec.grm"*)NONE(*#line 611.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 40, ( ( _, ( MlyValue.elem elem, elem1left, elem1right)) :: rest671)) => let val  result = MlyValue.elemseq ((*#line 197.17 "spec.grm"*)[elem](*#line 615.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, elem1left, elem1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.elemseq elemseq, _, elemseq1right)) :: _ :: ( _, ( MlyValue.elem elem, elem1left, _)) :: rest671)) => let val  result = MlyValue.elemseq ((*#line 198.31 "spec.grm"*)elem::elemseq(*#line 619.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, elem1left, elemseq1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 200.13 "spec.grm"*)Int(INT)(*#line 623.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, INT1left, INT1right), rest671)
end
|  ( 43, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 201.14 "spec.grm"*)Bool(true)(*#line 627.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 44, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 202.15 "spec.grm"*)Bool(false)(*#line 631.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 203.12 "spec.grm"*)Var(Var.fromString ID)(*#line 635.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, ID1left, ID1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.typespec ((*#line 205.28 "spec.grm"*)TypeSpec.T {name = Var.fromString ID,
                  params = Vector.new0 (), refty = refty}(*#line 639.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, ID1left, refty1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( MlyValue.paramseq paramseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.typespec ((*#line 207.51 "spec.grm"*)TypeSpec.T {
                  name = Var.fromString ID,
                  params = Vector.fromList paramseq, refty = refty}(*#line 644.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, LPAREN1left, refty1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.reftyatom reftyatom, reftyatom1left, reftyatom1right)) :: rest671)) => let val  result = MlyValue.refty ((*#line 212.20 "spec.grm"*)reftyatom(*#line 650.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, reftyatom1left, reftyatom1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.vartyatom vartyatom, vartyatom1left, _)) :: rest671)) => let val  result = MlyValue.refty ((*#line 213.32 "spec.grm"*)RefTy.Arrow (vartyatom, refty)(*#line 654.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, vartyatom1left, refty1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.basety basety, basety1left, basety1right)) :: rest671)) => let val  result = MlyValue.reftyatom ((*#line 215.21 "spec.grm"*)basety(*#line 658.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, basety1left, basety1right), rest671)
end
|  ( 51, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.vartyseq vartyseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.reftyatom ((*#line 216.38 "spec.grm"*)case vartyseq of
                          [(v, refty as RefTy.Base _)] => 
                              RefTy.alphaRenameToVar refty v
                        | [(v,refty)] => refty
                        | _ => RefTy.Tuple (Vector.fromList vartyseq)(*#line 662.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.basety basety, basety1left, basety1right)) :: rest671)) => let val  result = MlyValue.vartyatom ((*#line 222.21 "spec.grm"*)case basety of 
                      RefTy.Base (v,_,_) => (v,RefTy.alphaRename basety)
                    | _ => Error.bug "Impossible case of basety"(*#line 670.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, basety1left, basety1right), rest671)
end
|  ( 53, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.vartyseq vartyseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.vartyatom ((*#line 225.38 "spec.grm"*)case vartyseq of
                          [x] => x 
                        | _ => (genVar (), RefTy.Tuple 
                            (Vector.fromList vartyseq))(*#line 676.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.varty varty, varty1left, varty1right)) :: rest671)) => let val  result = MlyValue.vartyseq ((*#line 230.19 "spec.grm"*)[varty](*#line 683.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 26, ( result, varty1left, varty1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.vartyseq vartyseq, _, vartyseq1right)) :: _ :: ( _, ( MlyValue.varty varty, varty1left, _)) :: rest671)) => let val  result = MlyValue.vartyseq ((*#line 231.34 "spec.grm"*)varty :: vartyseq(*#line 687.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 26, ( result, varty1left, vartyseq1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.refty refty, refty1left, refty1right)) :: rest671)) => let val  result = MlyValue.varty ((*#line 233.16 "spec.grm"*)let open RefTy in case refty of 
                          Base (v,_,_) => (v,alphaRename refty)
                        | Tuple _ => (genVar (),refty)
                        | Arrow _ => (genVar (),refty)
                        end(*#line 691.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 27, ( result, refty1left, refty1right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.basety ((*#line 240.14 "spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), 
                Predicate.truee())(*#line 699.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 28, ( result, ID1left, ID1right), rest671)
end
|  ( 58, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.basety ((*#line 243.28 "spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), 
                Predicate.truee())(*#line 705.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 28, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 59, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.pred pred, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.basety ((*#line 246.38 "spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), pred)(*#line 711.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 28, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 60, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.pred ((*#line 250.15 "spec.grm"*)Predicate.truee()(*#line 716.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 29, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.rpatom rpatom, rpatom1left, rpatom1right)) :: rest671)) => let val  result = MlyValue.pred ((*#line 251.17 "spec.grm"*)Predicate.Rel rpatom(*#line 720.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 29, ( result, rpatom1left, rpatom1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: _ :: ( _, ( MlyValue.rpatom rpatom, rpatom1left, _)) :: rest671)) => let val  result = MlyValue.pred ((*#line 252.27 "spec.grm"*)Predicate.conjR (pred,rpatom)(*#line 724.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 29, ( result, rpatom1left, pred1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 254.31 "spec.grm"*)Predicate.RelPredicate.Eq(rexpr1,rexpr2)(*#line 728.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 31, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 255.30 "spec.grm"*)Predicate.RelPredicate.Sub(rexpr1,rexpr2)(*#line 732.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 31, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 256.32 "spec.grm"*)Predicate.RelPredicate.SubEq(rexpr1,rexpr2)(*#line 736.1 "spec.grm.sml"*)
)
 in ( LrTable.NT 31, ( result, rexpr1left, rexpr2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Spec_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun RELATION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun UNION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun CROSSPRD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun SUBSETEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun SUBSET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun EQUALOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun IFF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun CONJ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun STAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun LCURLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun RCURLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.ID i,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.INT i,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
end
end

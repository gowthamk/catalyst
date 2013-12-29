functor SpecLrValsFun (structure Token : TOKEN
                                structure Spec : SPEC_LANG) : Spec_LRVALS = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)(*  User declarations section for helper functions *)
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

(*#line 25.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\082\000\017\000\055\000\019\000\054\000\027\000\053\000\000\000\
\\001\000\002\000\099\000\003\000\098\000\018\000\097\000\027\000\096\000\
\\028\000\095\000\000\000\
\\001\000\002\000\099\000\003\000\098\000\027\000\096\000\028\000\095\000\000\000\
\\001\000\008\000\107\000\009\000\106\000\010\000\105\000\000\000\
\\001\000\010\000\035\000\017\000\034\000\019\000\033\000\027\000\032\000\000\000\
\\001\000\010\000\043\000\000\000\
\\001\000\010\000\102\000\017\000\034\000\019\000\033\000\027\000\032\000\000\000\
\\001\000\013\000\012\000\000\000\
\\001\000\013\000\062\000\000\000\
\\001\000\014\000\170\000\023\000\172\000\000\000\
\\001\000\014\000\010\000\000\000\
\\001\000\014\000\011\000\000\000\
\\001\000\017\000\016\000\027\000\015\000\000\000\
\\001\000\017\000\025\000\019\000\024\000\027\000\023\000\000\000\
\\001\000\017\000\034\000\019\000\033\000\027\000\032\000\000\000\
\\001\000\017\000\055\000\019\000\054\000\027\000\053\000\000\000\
\\001\000\017\000\072\000\000\000\
\\001\000\017\000\076\000\000\000\
\\001\000\018\000\026\000\000\000\
\\001\000\018\000\060\000\000\000\
\\001\000\018\000\067\000\000\000\
\\001\000\018\000\069\000\000\000\
\\001\000\018\000\078\000\000\000\
\\001\000\018\000\088\000\000\000\
\\001\000\018\000\100\000\000\000\
\\001\000\018\000\110\000\000\000\
\\001\000\018\000\112\000\000\000\
\\001\000\020\000\059\000\024\000\058\000\000\000\
\\001\000\020\000\104\000\000\000\
\\001\000\020\000\114\000\000\000\
\\001\000\020\000\121\000\000\000\
\\001\000\022\000\111\000\000\000\
\\001\000\023\000\037\000\000\000\
\\001\000\027\000\014\000\000\000\
\\001\000\027\000\036\000\000\000\
\\001\000\027\000\038\000\000\000\
\\001\000\027\000\042\000\000\000\
\\001\000\027\000\046\000\000\000\
\\001\000\027\000\048\000\000\000\
\\001\000\029\000\000\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\001\000\009\000\017\000\008\000\027\000\007\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\027\000\014\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\024\000\044\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\015\000\068\000\000\000\
\\143\000\015\000\068\000\017\000\071\000\019\000\033\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\021\000\075\000\000\000\
\\148\000\000\000\
\\149\000\021\000\075\000\000\000\
\\150\000\000\000\
\\151\000\016\000\073\000\017\000\072\000\000\000\
\\151\000\017\000\072\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\006\000\086\000\007\000\085\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\161\000\027\000\090\000\000\000\
\\162\000\015\000\113\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\015\000\061\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\012\000\103\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\"
val actionRowNumbers =
"\044\000\010\000\011\000\041\000\
\\040\000\007\000\033\000\012\000\
\\044\000\044\000\013\000\018\000\
\\049\000\004\000\034\000\043\000\
\\042\000\092\000\009\000\032\000\
\\086\000\094\000\035\000\013\000\
\\036\000\050\000\056\000\005\000\
\\052\000\045\000\054\000\037\000\
\\038\000\015\000\033\000\013\000\
\\027\000\089\000\019\000\090\000\
\\008\000\015\000\014\000\020\000\
\\059\000\021\000\060\000\070\000\
\\062\000\068\000\047\000\064\000\
\\017\000\015\000\022\000\088\000\
\\000\000\095\000\093\000\013\000\
\\013\000\073\000\053\000\016\000\
\\051\000\058\000\037\000\057\000\
\\023\000\037\000\079\000\063\000\
\\065\000\015\000\001\000\024\000\
\\006\000\098\000\028\000\003\000\
\\097\000\091\000\087\000\015\000\
\\015\000\061\000\055\000\025\000\
\\078\000\031\000\069\000\026\000\
\\080\000\082\000\085\000\029\000\
\\084\000\083\000\077\000\046\000\
\\015\000\000\000\096\000\015\000\
\\015\000\015\000\071\000\072\000\
\\076\000\066\000\030\000\002\000\
\\074\000\048\000\099\000\100\000\
\\101\000\102\000\067\000\075\000\
\\081\000\039\000"
val gotoT =
"\
\\001\000\121\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\011\000\000\000\
\\000\000\
\\003\000\015\000\004\000\002\000\022\000\001\000\000\000\
\\003\000\016\000\004\000\002\000\022\000\001\000\000\000\
\\023\000\020\000\025\000\019\000\027\000\018\000\028\000\017\000\000\000\
\\000\000\
\\005\000\025\000\000\000\
\\006\000\029\000\007\000\028\000\008\000\027\000\009\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\025\000\039\000\026\000\038\000\027\000\037\000\028\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\043\000\000\000\
\\011\000\045\000\000\000\
\\012\000\050\000\014\000\049\000\017\000\048\000\019\000\047\000\000\000\
\\005\000\054\000\000\000\
\\023\000\055\000\025\000\019\000\027\000\018\000\028\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\063\000\018\000\062\000\019\000\061\000\000\000\
\\006\000\064\000\007\000\028\000\008\000\027\000\009\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\068\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\072\000\000\000\
\\000\000\
\\014\000\063\000\018\000\075\000\019\000\061\000\000\000\
\\000\000\
\\000\000\
\\014\000\063\000\018\000\079\000\019\000\061\000\029\000\078\000\
\\031\000\077\000\000\000\
\\000\000\
\\000\000\
\\025\000\039\000\026\000\081\000\027\000\037\000\028\000\017\000\000\000\
\\023\000\082\000\025\000\019\000\027\000\018\000\028\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\085\000\000\000\
\\000\000\
\\000\000\
\\011\000\045\000\000\000\
\\015\000\087\000\000\000\
\\000\000\
\\000\000\
\\014\000\090\000\017\000\089\000\019\000\047\000\000\000\
\\020\000\092\000\021\000\091\000\000\000\
\\000\000\
\\006\000\099\000\007\000\028\000\008\000\027\000\009\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\063\000\018\000\106\000\019\000\061\000\000\000\
\\014\000\063\000\018\000\107\000\019\000\061\000\000\000\
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
\\012\000\113\000\014\000\049\000\017\000\048\000\019\000\047\000\000\000\
\\014\000\063\000\018\000\079\000\019\000\061\000\029\000\114\000\
\\031\000\077\000\000\000\
\\000\000\
\\014\000\063\000\018\000\115\000\019\000\061\000\000\000\
\\014\000\063\000\018\000\116\000\019\000\061\000\000\000\
\\014\000\063\000\018\000\117\000\019\000\061\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\118\000\000\000\
\\000\000\
\\020\000\092\000\021\000\120\000\000\000\
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
val numstates = 122
val numrules = 62
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
datatype svalue = VOID | ntVOID of unit | INT of  (int) | ID of  (string) | rpatom of  (Predicate.RelPredicate.t) | relpred of  (Predicate.RelPredicate.t) | pred of  (Predicate.t) | basety of  (RefinementType.t) | tyatom of  (RefinementType.t) | vartyatomseq of  ( ( Var.t * RefTy.t )  list) | vartyatom of  (Var.t*RefinementType.t) | reftyseq of  (RefinementType.t list) | refty of  (RefinementType.t) | typespec of  (TypeSpec.t) | elemseq of  (elem list) | elem of  (elem) | ratom of  (expr) | rexpr of  (expr) | ieatom of  (ieatom) | ieatoms of  (ieatom list) | varop of  (Var.t option) | instexpr of  (instexpr) | rterm of  (term) | iterm of  (term) | idseq of  (Var.t list) | conargs of  (Var.t vector) | valpat of  (Pat.value) | pat of  (Pat.t) | patmatch of  (Pat.t*term) | patmatchseq of  ( ( Pat.t * term )  list) | params of  (RelVar.t list) | reldec of  (StructuralRelation.t) | decsandtys of  (RelSpec.t) | spec of  (RelSpec.t) | start of  (RelSpec.t)
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
of  ( 0, ( ( _, ( MlyValue.spec spec, spec1left, spec1right)) :: rest671)) => let val  result = MlyValue.start ((*#line 100.15 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)spec(*#line 421.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, spec1left, spec1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.decsandtys decsandtys, decsandtys1left, decsandtys1right)) :: rest671)) => let val  result = MlyValue.spec ((*#line 102.21 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)decsandtys(*#line 425.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, decsandtys1left, decsandtys1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.decsandtys decsandtys, _, decsandtys1right)) :: _ :: ( _, ( MlyValue.reldec reldec, reldec1left, _)) :: rest671)) => let val  result = MlyValue.decsandtys ((*#line 105.18 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)case decsandtys of RelSpec.T ({reldecs,typespecs}) => 
                    RelSpec.T {reldecs = Vector.fromList (reldec ::
                      (Vector.toList reldecs)), typespecs = typespecs}(*#line 429.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, reldec1left, decsandtys1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.decsandtys decsandtys, _, decsandtys1right)) :: _ :: ( _, ( MlyValue.typespec typespec, typespec1left, _)) :: rest671)) => let val  result = MlyValue.decsandtys ((*#line 109.18 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)case decsandtys of RelSpec.T {reldecs,typespecs} => 
                    RelSpec.T {reldecs = reldecs, typespecs = 
                    Vector.fromList (typespec :: (Vector.toList typespecs))}(*#line 435.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, typespec1left, decsandtys1right), rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.decsandtys ((*#line 112.16 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)RelSpec.T {reldecs = Vector.fromList [],
                  typespecs = Vector.fromList []}(*#line 441.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 116.12 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)StructuralRelation.T {id=RelId.fromString ID,
                params = Vector.new0 (),
                map = Vector.fromList (List.map (patmatchseq,
                  fn (pat,rterm) => (SOME pat,rterm)))}(*#line 446.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, patmatchseq1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: _ :: ( _, ( MlyValue.params params, _, _)) :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 121.12 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)StructuralRelation.T {id=RelId.fromString ID,
                params = Vector.fromList params,
                map = Vector.fromList (List.map (patmatchseq,
                  fn (pat,rterm) => (SOME pat,rterm)))}(*#line 453.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, patmatchseq1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.iterm iterm, _, iterm1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 126.12 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)StructuralRelation.T{id=RelId.fromString ID,
                params = Vector.new0 (),
                map = Vector.fromList [(NONE,iterm)]}(*#line 460.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, iterm1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.iterm iterm, _, iterm1right)) :: _ :: _ :: ( _, ( MlyValue.params params, _, _)) :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 130.12 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)StructuralRelation.T{id=RelId.fromString ID,
                params = Vector.fromList params,
                map = Vector.fromList [(NONE,iterm)]}(*#line 466.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, iterm1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.params ((*#line 134.14 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)[RelVar.fromString ID](*#line 472.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.params params, _, params1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.params ((*#line 135.21 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)(RelVar.fromString ID)::params(*#line 476.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, ID1left, params1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: _ :: ( _, ( MlyValue.patmatch patmatch, patmatch1left, _)) :: rest671)) => let val  result = MlyValue.patmatchseq ((*#line 137.42 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)patmatch :: patmatchseq(*#line 480.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, patmatch1left, patmatchseq1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.patmatch patmatch, patmatch1left, patmatch1right)) :: rest671)) => let val  result = MlyValue.patmatchseq ((*#line 138.25 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)[patmatch](*#line 484.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, patmatch1left, patmatch1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.pat pat, pat1left, _)) :: rest671)) => let val  result = MlyValue.patmatch ((*#line 140.31 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)pat, Atom (Re rexpr)(*#line 488.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, pat1left, rexpr1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.pat ((*#line 147.11 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Pat.Con (Con.fromString ID, NONE)(*#line 492.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, ID1left, ID1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.valpat valpat, _, _)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.pat ((*#line 148.32 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Pat.Con (Con.fromString ID, SOME valpat)(*#line 496.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.valpat valpat, valpat1left, valpat1right)) :: rest671)) => let val  result = MlyValue.pat ((*#line 149.15 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Pat.Value valpat(*#line 500.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, valpat1left, valpat1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.idseq idseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.valpat ((*#line 151.31 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)case idseq of 
              [x] => Pat.Var x
            | _ => Pat.Tuple (Vector.fromList idseq)(*#line 504.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 18, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.idseq idseq, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.valpat ((*#line 154.31 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Pat.Record (Record.fromVector
          (Vector.fromList (List.map (idseq, fn x => 
            (Field.Symbol (Field.Symbol.fromString (Var.toString x)), x)))) )(*#line 510.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, LCURLY1left, RPAREN1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.idseq ((*#line 158.13 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)[Var.fromString ID](*#line 516.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, ID1left, ID1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.idseq idseq, _, idseq1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.idseq ((*#line 159.25 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)(Var.fromString ID)::idseq(*#line 520.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, ID1left, idseq1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ieatom ieatom, ieatom1left, ieatom1right)) :: rest671)) => let val  result = MlyValue.iterm ((*#line 161.17 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Atom ieatom(*#line 524.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, ieatom1left, ieatom1right), rest671)
end
|  ( 22, ( ( _, ( _, _, STAR1right)) :: ( _, ( MlyValue.instexpr instexpr, instexpr1left, _)) :: rest671)) => let val  result = MlyValue.iterm ((*#line 162.24 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Star instexpr(*#line 528.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, instexpr1left, STAR1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.instexpr ((*#line 165.16 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Relation (RelId.fromString ID)(*#line 532.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, ID1left, ID1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ieatoms ieatoms, _, ieatoms1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.instexpr ((*#line 166.24 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Inst {args = 
                Vector.fromList ieatoms, rel = RelId.fromString ID}(*#line 536.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, ID1left, ieatoms1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ieatom ieatom, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.ieatoms ((*#line 169.33 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)[ieatom](*#line 541.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ieatoms ieatoms, _, ieatoms1right)) :: _ :: ( _, ( MlyValue.ieatom ieatom, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.ieatoms ((*#line 170.41 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)ieatom :: ieatoms(*#line 545.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, LBRACE1left, ieatoms1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.instexpr instexpr, instexpr1left, instexpr1right)) :: rest671)) => let val  result = MlyValue.ieatom ((*#line 172.20 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Ie instexpr(*#line 549.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, instexpr1left, instexpr1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ratom ratom, ratom1left, ratom1right)) :: rest671)) => let val  result = MlyValue.ieatom ((*#line 173.17 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Re ratom(*#line 553.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, ratom1left, ratom1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 175.31 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)X(ratom,rexpr)(*#line 557.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 176.28 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)U(ratom,rexpr)(*#line 561.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.ratom ratom, ratom1left, ratom1right)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 177.16 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)ratom(*#line 565.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, ratom1left, ratom1right), rest671)
end
|  ( 32, ( ( _, ( _, _, RCURLY1right)) :: _ :: _ :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 179.38 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)T (Vector.fromList [])(*#line 569.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RCURLY1right)) :: _ :: ( _, ( MlyValue.elemseq elemseq, _, _)) :: _ :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 181.12 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)T (Vector.fromList elemseq)(*#line 573.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.varop varop, _, _)) :: _ :: ( _, ( MlyValue.instexpr instexpr, instexpr1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 182.39 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)case (instexpr,varop) of 
            (Relation rid, NONE) => R1 (RelVar.fromString 
                (RelId.toString rid))
          | (_,SOME v) => R2 (instexpr,v)
          | _ => raise (Fail "Missing arguments for an \
            \ instantiated relation\n")(*#line 577.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, instexpr1left, RPAREN1right), rest671)
end
|  ( 35, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.rexpr rexpr, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 188.30 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)rexpr(*#line 586.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.varop ((*#line 190.13 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)SOME (Var.fromString ID)(*#line 590.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, ID1left, ID1right), rest671)
end
|  ( 37, ( rest671)) => let val  result = MlyValue.varop ((*#line 191.13 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)NONE(*#line 594.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 38, ( ( _, ( MlyValue.elem elem, elem1left, elem1right)) :: rest671)) => let val  result = MlyValue.elemseq ((*#line 193.17 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)[elem](*#line 598.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, elem1left, elem1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.elemseq elemseq, _, elemseq1right)) :: _ :: ( _, ( MlyValue.elem elem, elem1left, _)) :: rest671)) => let val  result = MlyValue.elemseq ((*#line 194.31 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)elem::elemseq(*#line 602.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, elem1left, elemseq1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 196.13 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Int(INT)(*#line 606.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, INT1left, INT1right), rest671)
end
|  ( 41, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 197.14 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Bool(true)(*#line 610.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 42, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 198.15 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Bool(false)(*#line 614.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 199.12 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Var(Var.fromString ID)(*#line 618.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, ID1left, ID1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.typespec ((*#line 201.28 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)TypeSpec.T {name = Var.fromString ID,
                  params = Vector.new0 (), refty = refty}(*#line 622.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, ID1left, refty1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( MlyValue.params params, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.typespec ((*#line 203.49 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)TypeSpec.T {
                  name = Var.fromString ID,
                  params = Vector.fromList params, refty = refty}(*#line 627.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, LPAREN1left, refty1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.tyatom tyatom, tyatom1left, tyatom1right)) :: rest671)) => let val  result = MlyValue.refty ((*#line 208.17 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)case tyatom of 
            RefTy.Base _ => RefTy.alphaRename tyatom | _ => tyatom(*#line 633.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, tyatom1left, tyatom1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.vartyatom vartyatom, vartyatom1left, _)) :: rest671)) => let val  result = MlyValue.refty ((*#line 210.32 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)RefinementType.Arrow(vartyatom,refty)(*#line 638.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, vartyatom1left, refty1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.tyatom tyatom, tyatom1left, tyatom1right)) :: rest671)) => let val  result = MlyValue.vartyatom ((*#line 212.21 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)let open RefTy in case tyatom of 
                      Base (v,_,_) => (v,alphaRename tyatom)
                    | Tuple _ => (genVar (),tyatom)
                    | Arrow _ => (genVar (),tyatom)
                    end(*#line 642.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, tyatom1left, tyatom1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.vartyatom vartyatom, vartyatom1left, vartyatom1right)) :: rest671)) => let val  result = MlyValue.vartyatomseq ((*#line 218.27 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)[vartyatom](*#line 650.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, vartyatom1left, vartyatom1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.vartyatomseq vartyatomseq, _, vartyatomseq1right)) :: _ :: ( _, ( MlyValue.vartyatom vartyatom, vartyatom1left, _)) :: rest671)) => let val  result = MlyValue.vartyatomseq ((*#line 219.46 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)vartyatom ::
                  vartyatomseq(*#line 654.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, vartyatom1left, vartyatomseq1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.basety basety, basety1left, basety1right)) :: rest671)) => let val  result = MlyValue.tyatom ((*#line 222.18 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)basety(*#line 659.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 26, ( result, basety1left, basety1right), rest671)
end
|  ( 52, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.vartyatomseq vartyatomseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.tyatom ((*#line 223.38 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)case vartyatomseq of 
                  [(v,refty)] => refty
                | _ => RefTy.Tuple (Vector.fromList vartyatomseq)(*#line 663.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 26, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.basety ((*#line 228.14 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), 
                Predicate.truee())(*#line 669.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 27, ( result, ID1left, ID1right), rest671)
end
|  ( 54, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.basety ((*#line 231.28 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), 
                Predicate.truee())(*#line 675.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 27, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 55, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.pred pred, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.basety ((*#line 234.38 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), pred)(*#line 681.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 27, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 56, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.pred ((*#line 238.15 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.truee()(*#line 686.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 28, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.rpatom rpatom, rpatom1left, rpatom1right)) :: rest671)) => let val  result = MlyValue.pred ((*#line 239.17 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.Rel rpatom(*#line 690.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 28, ( result, rpatom1left, rpatom1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: _ :: ( _, ( MlyValue.rpatom rpatom, rpatom1left, _)) :: rest671)) => let val  result = MlyValue.pred ((*#line 240.27 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.conjR (pred,rpatom)(*#line 694.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 28, ( result, rpatom1left, pred1right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 242.31 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.Eq(rexpr1,rexpr2)(*#line 698.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 30, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 243.30 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.Sub(rexpr1,rexpr2)(*#line 702.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 30, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 244.32 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.SubEq(rexpr1,rexpr2)(*#line 706.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 30, ( result, rexpr1left, rexpr2right), rest671)
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

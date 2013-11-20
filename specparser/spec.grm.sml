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
val defaultCons = Con.default

(*#line 14.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\061\000\017\000\039\000\019\000\038\000\023\000\057\000\000\000\
\\001\000\002\000\075\000\003\000\074\000\018\000\073\000\023\000\072\000\
\\024\000\071\000\000\000\
\\001\000\002\000\075\000\003\000\074\000\023\000\072\000\024\000\071\000\000\000\
\\001\000\008\000\081\000\009\000\080\000\010\000\079\000\000\000\
\\001\000\010\000\025\000\017\000\024\000\023\000\023\000\000\000\
\\001\000\010\000\031\000\000\000\
\\001\000\010\000\063\000\000\000\
\\001\000\013\000\011\000\000\000\
\\001\000\014\000\009\000\000\000\
\\001\000\014\000\010\000\000\000\
\\001\000\016\000\054\000\017\000\053\000\000\000\
\\001\000\017\000\020\000\019\000\019\000\023\000\018\000\000\000\
\\001\000\017\000\024\000\023\000\023\000\000\000\
\\001\000\017\000\039\000\019\000\038\000\023\000\037\000\000\000\
\\001\000\017\000\039\000\019\000\038\000\023\000\057\000\000\000\
\\001\000\017\000\053\000\000\000\
\\001\000\017\000\055\000\000\000\
\\001\000\018\000\043\000\000\000\
\\001\000\018\000\047\000\000\000\
\\001\000\018\000\076\000\000\000\
\\001\000\018\000\083\000\000\000\
\\001\000\018\000\085\000\000\000\
\\001\000\018\000\086\000\000\000\
\\001\000\020\000\042\000\022\000\041\000\000\000\
\\001\000\020\000\078\000\000\000\
\\001\000\020\000\088\000\000\000\
\\001\000\020\000\094\000\000\000\
\\001\000\023\000\012\000\000\000\
\\001\000\023\000\027\000\000\000\
\\001\000\023\000\033\000\000\000\
\\001\000\023\000\065\000\000\000\
\\001\000\023\000\068\000\000\000\
\\001\000\025\000\000\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\001\000\008\000\023\000\007\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\022\000\030\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\017\000\050\000\023\000\049\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\015\000\084\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\006\000\052\000\007\000\051\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\015\000\087\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\021\000\026\000\000\000\
\\131\000\000\000\
\\132\000\015\000\044\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\012\000\077\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\"
val actionRowNumbers =
"\037\000\008\000\009\000\034\000\
\\033\000\007\000\027\000\037\000\
\\037\000\011\000\004\000\036\000\
\\035\000\070\000\066\000\065\000\
\\072\000\028\000\011\000\041\000\
\\038\000\005\000\029\000\013\000\
\\011\000\023\000\017\000\068\000\
\\012\000\013\000\018\000\044\000\
\\054\000\051\000\039\000\010\000\
\\016\000\014\000\067\000\000\000\
\\073\000\071\000\011\000\040\000\
\\043\000\006\000\045\000\046\000\
\\030\000\014\000\014\000\031\000\
\\050\000\001\000\019\000\015\000\
\\076\000\024\000\003\000\075\000\
\\069\000\013\000\020\000\048\000\
\\052\000\053\000\021\000\022\000\
\\059\000\061\000\064\000\025\000\
\\063\000\062\000\058\000\000\000\
\\074\000\014\000\014\000\014\000\
\\042\000\047\000\030\000\057\000\
\\026\000\002\000\055\000\077\000\
\\078\000\079\000\080\000\049\000\
\\056\000\060\000\032\000"
val gotoT =
"\
\\001\000\094\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\011\000\004\000\002\000\015\000\001\000\000\000\
\\003\000\012\000\004\000\002\000\015\000\001\000\000\000\
\\016\000\015\000\018\000\014\000\019\000\013\000\000\000\
\\005\000\020\000\006\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\027\000\017\000\026\000\018\000\014\000\019\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\030\000\000\000\
\\010\000\034\000\011\000\033\000\012\000\032\000\000\000\
\\016\000\038\000\018\000\014\000\019\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\043\000\006\000\019\000\000\000\
\\010\000\044\000\011\000\033\000\012\000\032\000\000\000\
\\000\000\
\\008\000\046\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\054\000\012\000\032\000\000\000\
\\000\000\
\\011\000\058\000\012\000\032\000\020\000\057\000\022\000\056\000\000\000\
\\000\000\
\\000\000\
\\016\000\027\000\017\000\060\000\018\000\014\000\019\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\062\000\000\000\
\\011\000\064\000\012\000\032\000\000\000\
\\011\000\065\000\012\000\032\000\000\000\
\\000\000\
\\000\000\
\\013\000\068\000\014\000\067\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\080\000\011\000\033\000\012\000\032\000\000\000\
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
\\011\000\058\000\012\000\032\000\020\000\087\000\022\000\056\000\000\000\
\\000\000\
\\011\000\088\000\012\000\032\000\000\000\
\\011\000\089\000\012\000\032\000\000\000\
\\011\000\090\000\012\000\032\000\000\000\
\\000\000\
\\000\000\
\\009\000\091\000\000\000\
\\000\000\
\\000\000\
\\013\000\068\000\014\000\093\000\000\000\
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
val numstates = 95
val numrules = 48
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
datatype svalue = VOID | ntVOID of unit | INT of  (int) | ID of  (string) | rpatom of  (Predicate.RelPredicate.t) | relpred of  (Predicate.RelPredicate.t) | pred of  (Predicate.t) | basety of  (RefinementType.t) | tyatom of  (RefinementType.t) | reftyseq of  (RefinementType.t list) | refty of  (RefinementType.t) | typespec of  (TypeSpec.t) | elemseq of  (elem list) | elem of  (elem) | ratom of  (expr) | rexpr of  (expr) | rterm of  (term) | idseq of  (Var.t list) | conargs of  (Var.t vector) | conpat of  (Con.t*Var.t vector option) | patmatch of  (Con.t*Var.t vector option*term) | patmatchseq of  ( ( Con.t * Var.t vector option * term )  list) | reldec of  (StructuralRelation.t) | decsandtys of  (RelSpec.t) | spec of  (RelSpec.t) | start of  (RelSpec.t)
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
fn (T 24) => true | _ => false
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
  | (T 20) => "ARROW"
  | (T 21) => "PIPE"
  | (T 22) => "ID"
  | (T 23) => "INT"
  | (T 24) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 24) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.spec spec, spec1left, spec1right)) :: rest671)) => let val  result = MlyValue.start ((*#line 75.15 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)spec(*#line 348.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, spec1left, spec1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.decsandtys decsandtys, decsandtys1left, decsandtys1right)) :: rest671)) => let val  result = MlyValue.spec ((*#line 77.21 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)decsandtys(*#line 352.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, decsandtys1left, decsandtys1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.decsandtys decsandtys, _, decsandtys1right)) :: _ :: ( _, ( MlyValue.reldec reldec, reldec1left, _)) :: rest671)) => let val  result = MlyValue.decsandtys ((*#line 80.18 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)case decsandtys of RelSpec.T ({reldecs,typespecs}) => 
                    RelSpec.T {reldecs = Vector.fromList (reldec ::
                      (Vector.toList reldecs)), typespecs = typespecs}(*#line 356.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, reldec1left, decsandtys1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.decsandtys decsandtys, _, decsandtys1right)) :: _ :: ( _, ( MlyValue.typespec typespec, typespec1left, _)) :: rest671)) => let val  result = MlyValue.decsandtys ((*#line 84.18 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)case decsandtys of RelSpec.T {reldecs,typespecs} => 
                    RelSpec.T {reldecs = reldecs, typespecs = 
                    Vector.fromList (typespec :: (Vector.toList typespecs))}(*#line 362.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, typespec1left, decsandtys1right), rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.decsandtys ((*#line 87.16 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)RelSpec.T {reldecs = Vector.fromList [],
                  typespecs = Vector.fromList []}(*#line 368.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 91.12 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)StructuralRelation.T {id=RelId.fromString ID,
                map = Vector.fromList patmatchseq}(*#line 373.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, patmatchseq1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.rterm rterm, _, rterm1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 94.12 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)StructuralRelation.T{id=RelId.fromString ID,
                map = Vector.fromList [(defaultCons,NONE,rterm)]}(*#line 378.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, rterm1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: _ :: ( _, ( MlyValue.patmatch patmatch, patmatch1left, _)) :: rest671)) => let val  result = MlyValue.patmatchseq ((*#line 97.42 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)patmatch :: patmatchseq(*#line 383.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, patmatch1left, patmatchseq1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.patmatch patmatch, patmatch1left, patmatch1right)) :: rest671)) => let val  result = MlyValue.patmatchseq ((*#line 98.25 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)[patmatch](*#line 387.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, patmatch1left, patmatch1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.rterm rterm, _, rterm1right)) :: _ :: _ :: ( _, ( MlyValue.conpat conpat, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.patmatch ((*#line 101.16 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)case conpat of (c,vlop) => (c,vlop,rterm)(*#line 391.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LPAREN1left, rterm1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.rterm rterm, _, rterm1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.patmatch ((*#line 102.30 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)(Con.fromString ID,NONE,rterm)(*#line 395.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, ID1left, rterm1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.conpat ((*#line 104.15 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Con.fromString ID, NONE(*#line 399.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.conargs conargs, _, conargs1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.conpat ((*#line 105.23 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Con.fromString ID, SOME conargs(*#line 403.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, ID1left, conargs1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.conargs ((*#line 107.15 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Vector.fromList [Var.fromString ID](*#line 407.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, ID1left, ID1right), rest671)
end
|  ( 14, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.idseq idseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.conargs ((*#line 108.32 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Vector.fromList idseq(*#line 411.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.idseq ((*#line 110.13 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)[Var.fromString ID](*#line 415.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.idseq idseq, _, idseq1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.idseq ((*#line 111.25 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)(Var.fromString ID)::idseq(*#line 419.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, idseq1right), rest671)
end
|  ( 17, ( ( _, ( _, _, STAR1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.rterm ((*#line 113.18 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Star(RelId.fromString ID)(*#line 423.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, ID1left, STAR1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.rexpr rexpr, rexpr1left, rexpr1right)) :: rest671)) => let val  result = MlyValue.rterm ((*#line 114.15 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Expr(rexpr)(*#line 427.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, rexpr1left, rexpr1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 116.31 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)X(ratom,rexpr)(*#line 431.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 117.28 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)U(ratom,rexpr)(*#line 435.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ratom ratom, ratom1left, ratom1right)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 118.16 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)ratom(*#line 439.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, ratom1left, ratom1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RCURLY1right)) :: _ :: _ :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 120.38 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)T(Vector.fromList [])(*#line 443.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 23, ( ( _, ( _, _, RCURLY1right)) :: _ :: ( _, ( MlyValue.elemseq elemseq, _, _)) :: _ :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 121.46 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)T(Vector.fromList elemseq)(*#line 447.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 24, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 122.30 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)R(RelId.fromString ID1, Var.fromString ID2)(*#line 451.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.rexpr rexpr, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 123.30 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)rexpr(*#line 455.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.elem elem, elem1left, elem1right)) :: rest671)) => let val  result = MlyValue.elemseq ((*#line 125.17 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)[elem](*#line 459.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, elem1left, elem1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.elemseq elemseq, _, elemseq1right)) :: _ :: ( _, ( MlyValue.elem elem, elem1left, _)) :: rest671)) => let val  result = MlyValue.elemseq ((*#line 126.31 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)elem::elemseq(*#line 463.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, elem1left, elemseq1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 128.13 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Int(INT)(*#line 467.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, INT1left, INT1right), rest671)
end
|  ( 29, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 129.14 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Bool(true)(*#line 471.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 30, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 130.15 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Bool(false)(*#line 475.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 131.12 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Var(Var.fromString ID)(*#line 479.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, ID1left, ID1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.typespec ((*#line 133.28 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)TypeSpec.T ((Var.fromString ID),refty)(*#line 483.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, ID1left, refty1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.tyatom tyatom, tyatom1left, tyatom1right)) :: rest671)) => let val  result = MlyValue.refty ((*#line 135.17 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)tyatom(*#line 487.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, tyatom1left, tyatom1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.tyatom tyatom1, tyatom1left, _)) :: rest671)) => let val  result = MlyValue.refty ((*#line 136.29 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)RefinementType.Arrow(tyatom1,refty)(*#line 491.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, tyatom1left, refty1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.refty refty, refty1left, refty1right)) :: rest671)) => let val  result = MlyValue.reftyseq ((*#line 138.19 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)[refty](*#line 495.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, refty1left, refty1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.reftyseq reftyseq, _, reftyseq1right)) :: _ :: ( _, ( MlyValue.refty refty, refty1left, _)) :: rest671)) => let val  result = MlyValue.reftyseq ((*#line 139.34 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)refty::reftyseq(*#line 499.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, refty1left, reftyseq1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.basety basety, basety1left, basety1right)) :: rest671)) => let val  result = MlyValue.tyatom ((*#line 141.18 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)basety(*#line 503.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, basety1left, basety1right), rest671)
end
|  ( 38, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.reftyseq reftyseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.tyatom ((*#line 142.34 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)case reftyseq of 
                  [refty] => refty
                | _ => RefinementType.Tuple 
                  (Vector.fromList reftyseq)(*#line 507.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.basety ((*#line 148.14 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), 
                Predicate.truee())(*#line 514.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, ID1left, ID1right), rest671)
end
|  ( 40, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.basety ((*#line 151.28 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), 
                Predicate.truee())(*#line 520.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 41, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.pred pred, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.basety ((*#line 154.38 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), pred)(*#line 526.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 42, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.pred ((*#line 158.15 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.truee()(*#line 531.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.rpatom rpatom, rpatom1left, rpatom1right)) :: rest671)) => let val  result = MlyValue.pred ((*#line 159.17 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.Rel rpatom(*#line 535.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, rpatom1left, rpatom1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: _ :: ( _, ( MlyValue.rpatom rpatom, rpatom1left, _)) :: rest671)) => let val  result = MlyValue.pred ((*#line 160.27 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.conjR (pred,rpatom)(*#line 539.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, rpatom1left, pred1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 162.31 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.Eq(rexpr1,rexpr2)(*#line 543.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 163.30 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.Sub(rexpr1,rexpr2)(*#line 547.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 164.32 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.SubEq(rexpr1,rexpr2)(*#line 551.1 "/Users/gowtham/git/relspecs/implementation/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, rexpr1left, rexpr2right), rest671)
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
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.ID i,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.INT i,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
end
end
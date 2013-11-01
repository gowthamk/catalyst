signature Z3_CTX_STRUCTS = 
sig
  include Z3_FFI_INTERFACE
end
signature Z3_CTX = 
sig
  include Z3_CTX_STRUCTS
  val mkDefaultContext : unit -> z3_context
  val checkContext : z3_context -> int
  val delContext : z3_context -> unit
end
signature Z3_ENCODE_STRUCTS = 
sig
  include Z3_FFI_INTERFACE
  val freshContext : z3_context
end
signature Z3_ENCODE = 
sig
  include Z3_ENCODE_STRUCTS
  type sort
  type set
  type ast
  type struc_rel
  type assertion
  exception InvalidOperation
  val bool_sort : sort
  val int_sort : sort
  val mkUninterpretedSort : unit -> sort
  val mkConst : (string * sort) -> ast
  val mkStrucRel : string * sort vector -> struc_rel
  val mkEmptySet : (string * sort vector) -> set
  val mkSingletonSet : (string * ast vector) -> set
  val mkUnion : set * set -> set
  val mkCrossPrd : set * set -> set 
  val mkSetEqAssertion : set * set -> assertion
  val mkConstEqAssertion : ast * ast -> assertion
  val mkNot : assertion -> assertion
  val dischargeAssertion : assertion -> unit
end

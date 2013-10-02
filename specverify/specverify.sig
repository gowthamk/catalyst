signature SPEC_VERIFY_STRUCTS =
sig
  structure VE : VAR_ENV
  structure ANormalCoreML : A_NORMAL_CORE_ML
end
signature SPEC_VERIFY = 
sig
  include SPEC_VERIFY_STRUCTS
  structure VC : VERIFICATION_CONDITION
  (*
   * Verifies program in the context of var env.
   * Returns verification conditions.
   *)
  val doIt : VE.t * ANormalCoreML.Program.t -> VC.t vector
end

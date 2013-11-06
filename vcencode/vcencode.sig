signature VC_ENCODE_STRUCTS =
sig
  structure VC : VERIFICATION_CONDITION
end
signature VC_ENCODE =
sig
  include VC_ENCODE_STRUCTS
  datatype result = Success | Undef | Failure
  val discharge : VC.t -> result
end

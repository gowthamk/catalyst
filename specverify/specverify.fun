functor SpecVerify (S : SPEC_VERIFY_STRUCTS) : SPEC_VERIFY = 
struct
  open S
 
  structure SpecLang = VE.SpecLang
  structure VC = VerificationCondition (open SpecLang
                                        structure VE = VE
                                        structure ANormalCoreML = 
                                          ANormalCoreML)
  open SpecLang
  open ANormalCoreML

  fun doIt (ve, Program.T{decs}) =
    raise (Fail "unimpl")

end

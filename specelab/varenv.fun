functor VarEnv (S : VAR_ENV_STRUCTS) : VAR_ENV = 
struct
  open S

  type tyscheme = RefinementTypeScheme.t

  val varStrEq = fn (v,v') => (Var.toString v) = (Var.toString v')

  structure Key:KEY = 
  struct
    type t = Var.t
    val equal = varStrEq
    val toString = Var.toString
  end
  structure Value:VALUE = 
  struct
    type t = tyscheme
    val toString = RefinementTypeScheme.toString
  end

  structure VarMap = ApplicativeMap (structure Key = Key
                                     structure Value = Value)

  exception VarNotFound of Var.t

  type t = VarMap.t

  val empty = VarMap.empty

  val mem = VarMap.mem

  fun find env var = VarMap.find env var 
    handle (VarMap.KeyNotFound k) => raise (VarNotFound k)

  val add = fn env => fn (var,tys) => VarMap.add env var tys 

  val remove = VarMap.remove

  val toVector = VarMap.toVector

  val toString = VarMap.toString
end

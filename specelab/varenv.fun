functor VarEnv (S : VAR_ENV_STRUCTS) : VAR_ENV = 
struct
  open S

  type tyscheme = RefinementTypeScheme.t

  val varStrEq = fn (v,v') => (Var.toString v) = (Var.toString v')

  structure Key:KEY = 
  struct
    type t = Var.t
    val equal = varStrEq
  end

  structure VarMap = ApplicativeMap (structure Key = Key)

  exception VarNotFound

  type t = tyscheme VarMap.t

  val empty = VarMap.empty

  val mem = VarMap.mem

  fun find env var = VarMap.find env var 
    handle KeyNotFound => raise VarNotFound

  val add = fn env => fn (var,tys) => VarMap.add env var tys 

  val remove = VarMap.remove

  val toVector = VarMap.toVector
end

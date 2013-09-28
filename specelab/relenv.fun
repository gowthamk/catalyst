functor RelEnv (S : REL_ENV_STRUCTS) : REL_ENV = 
struct
  open S

  type reldesc = { ty : unit,
                  map : (Con.t * Var.t vector option * RelLang.expr) 
                    vector}

  val relIdStrEq = fn (v,v') => 
    (RelLang.RelId.toString v) = (RelLang.RelId.toString v')

  structure Key:KEY = 
  struct
    type t = RelLang.RelId.t
    val equal = relIdStrEq
  end

  structure RelMap = ApplicativeMap (structure Key = Key)

  exception RelNotFound

  type t = reldesc RelMap.t

  val empty = RelMap.empty

  val mem = RelMap.mem

  fun find env relId = RelMap.find env relId 
    handle KeyNotFound => raise RelNotFound

  val add = fn env => fn (var,tys) => RelMap.add env var tys 

  val remove = RelMap.remove

  val toVector = RelMap.toVector
end

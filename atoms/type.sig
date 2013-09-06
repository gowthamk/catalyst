signature TYPE_DESC_STRUCTS = 
 sig
    structure Tyvar : TYVAR
    structure Tycon : TYCON
 end
   
signature TYPE_DESC = 
   sig
      include TYPE_DESC_STRUCTS
      
      datatype t =
	  	Tunknown
	  | Tvar of Tyvar.t
	  | Tarrow of t * t
	  | Ttuple of t list
	  | Tconstr of Tycon.t * t list
	  | Tfield of string * t
	  
	  val makeTarrow : (t * t) -> t
	  val makeTconstr: (Tycon.t * t list) -> t
	  val makeTvar: Tyvar.t -> t
	  val makeTfield: (string * t) -> t
	  val makeTtuple: t list -> t
	  val makeTunknown: unit -> t
    val toString : t -> string
	  val sametype : (t * t) -> bool
    val isWidthSubType : (t * t) -> bool
   end

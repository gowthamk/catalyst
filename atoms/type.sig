signature TYPE_DESC_STRUCTS = 
 sig
    structure Tyvar : TYVAR
    structure Tycon : TYCON
    structure Field : FIELD
    structure Record : RECORD
    sharing Field = Record.Field
 end
   
signature TYPE_DESC = 
   sig
      include TYPE_DESC_STRUCTS
      
      datatype t =
	  	Tunknown
	  | Tvar of Tyvar.t
	  | Tarrow of t * t
	  | Trecord of t Record.t
	  | Tconstr of Tycon.t * t list
	  
	  val makeTarrow : (t * t) -> t
	  val makeTconstr: (Tycon.t * t list) -> t
	  val makeTvar: Tyvar.t -> t
	  val makeTrecord: (Field.t * t) vector -> t
	  val makeTunknown: unit -> t
    val toString : t -> string
	  val sametype : (t * t) -> bool
    val isWidthSubType : (t * t) -> bool
   end

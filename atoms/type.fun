functor TypeDesc (S: TYPE_DESC_STRUCTS): TYPE_DESC = 
	struct
	
		open S
	
		datatype t =
			  	Tunknown
			  | Tvar of Tyvar.t
			  | Tarrow of t * t
			  | Trecord of t Record.t
			  | Tconstr of Tycon.t * t list
			  
	 	fun makeTarrow (tdesc1, tdesc2) = Tarrow (tdesc1, tdesc2)

	 	fun makeTconstr (cons, tdlist) = Tconstr (cons, tdlist)

	 	fun makeTvar tvar = Tvar tvar

    val makeTrecord = Trecord o Record.fromVector

	 	fun makeTunknown () = Tunknown

    fun toString t = case t of
        Tunknown => "<?>"
      | Tvar v  => Tyvar.toString v
      | Tarrow (t1,t2)=> "("^(toString t1)^") -> ("^(toString t2)^")"
      | Trecord tdrec => "{" ^ (Vector.toString (fn (lbl,td) => 
          (Field.toString lbl) ^ " : " ^ (toString td)) 
          (Record.toVector tdrec)) ^ "}"
      | Tconstr (tc,tdl) => "(" ^ (List.toString toString tdl) ^") " 
          ^ (Tycon.toString tc)

    fun sameType (t1,t2) = 
      let 
        fun sameTypes (tl1,tl2) = (List.length tl1 = List.length tl2) 
          andalso List.fold2 (tl1,tl2,true, fn(t1,t2,flag) => 
            (flag andalso sameType (t1,t2)))
      in
      case (t1,t2) of
          (Tunknown,Tunknown) => true
        | (Tvar v1, Tvar v2 ) => Tyvar.equals (v1,v2)
        | (Tarrow (tda1,tdr1), Tarrow (tda2,tdr2)) => 
            (sameType (tda1,tda2)) andalso
            (sameType (tdr1,tdr2))
        | (Trecord tdrec1, Trecord tdrec2) => Vector.forall (
            Record.toVector tdrec1, fn (l1,t2) => 
              Vector.exists (Record.toVector tdrec2, fn (l2,t2) => 
                (Field.toString l1 = Field.toString l2) 
                andalso sameType (t1,t2)))
        | (Tconstr (tycon1,td1), Tconstr (tycon2,td2)) => 
            Tycon.equals (tycon1,tycon2) andalso
            sameTypes (td1,td2)
        | (_,_) => false
      end

    fun unifiable (t1,t2) = true

	end
	

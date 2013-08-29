functor SpecLang (S : SPEC_LANG_STRUCTS) : SPEC_LANG = 
struct
  open S

  structure TypeDesc = TypeDesc (open S)

  structure RelLang =
  struct
    structure RelId = Var

    datatype elem = Int of int
                  | Bool of bool
                  | Var of Var.t
    datatype expr = T of elem list
                  | X of expr * expr
                  | U of expr * expr
                  | R of RelId.t * Var.t
    datatype term = Expr of expr
                  | Star of RelId.t 
    val elemToString = fn el => case el of
        Int i => Int.toString i
      | Bool b => Bool.toString b
      | Var v => Var.toString v

    fun exprToString exp = case exp of
        T (ellist) => "{(" ^ (List.fold (ellist,"",fn(e,acc) => 
          (elemToString e) ^ acc)) ^ ")}"
      | X (e1,e2) => "(" ^ (exprToString e1) ^ " X " 
          ^ (exprToString e2) ^ ")"
      | U (e1,e2) => "(" ^ (exprToString e1) ^ " U " 
          ^ (exprToString e2) ^ ")"
      | R (rel,arg) => (RelId.toString rel) ^ "(" ^ (Var.toString arg) ^ ")"
    
    val exprToString = exprToString

    val termToString = fn trm => case trm of
        Expr e => exprToString e
      | Star r => (RelId.toString r) ^ "*"
  end

  structure StructuralRelation =
  struct
    datatype t = T of {id : RelLang.RelId.t,
                       ty : unit,
                       map : (Con.t * Var.t list option * RelLang.term) list}
    val toString = fn T{id,ty,map} =>
      let val relid = Var.toString id
          val conmap = List.fold (map,"\n",fn((c,vlo,trm),acc) => 
            let val cstr = Con.toString c
                val vseq = case vlo of NONE => ""
                  | SOME vl => "(" ^ (List.fold (vl,"",fn(v,acc) => 
                      (Var.toString v) ^ "," ^ acc)) ^ ")"
                val trmstr = RelLang.termToString trm
            in
              "| " ^ cstr ^ vseq  ^ " = " ^ trmstr ^ "\n" ^ acc 
            end)
      in
        "relation " ^ relid ^ conmap
      end
  end

  structure Predicate =
  struct
    structure BasePredicate =
    struct
      datatype int_expr =   Plus of expr * expr
                          | Minus of expr * expr
                          | Mult of expr * expr
      and expr =  Int of int
                | Bool of bool
                | Var of Var.t
                | IntExpr of int_expr
      datatype t =  True 
                  | Conj of t * t
                  | Iff of t * t
                  | Eq of expr * expr
      val toString = fn _ => "T"
    end
    structure RelPredicate =
    struct
      type expr = RelLang.expr
      datatype t = True
                 | Eq of expr * expr
                 | Sub of expr * expr
                 | SubEq of expr * expr
                 | Conj of t * t
      fun toString rp = case rp of
          True => "T"
        | Eq (e1,e2) => (RelLang.exprToString e1) ^ " = "
            ^ (RelLang.exprToString e2)
        | Sub (e1,e2) => (RelLang.exprToString e1) ^ " C "
            ^ (RelLang.exprToString e2)
        | SubEq (e1,e2) => (RelLang.exprToString e1) ^ " C= "
            ^ (RelLang.exprToString e2)
        | Conj (e1,e2) => (toString e1) ^ " /\\ " ^ (toString e2)

      val toString = toString
    end
    datatype t =  T of BasePredicate.t * RelPredicate.t
    val toString = fn T(bp,rp) => "(" ^ (BasePredicate.toString bp) 
        ^ "," ^ (RelPredicate.toString rp)^ ")"
  end

  structure RefinementType =
  struct
    datatype t = Base of Var.t * TypeDesc.t * Predicate.t
               | Arrow of t*t
    fun toString rty = case rty of
          Base(var,_,pred) => "{" ^ (Var.toString var) ^ " | "
              ^ (Predicate.toString pred) ^ "}"
        | Arrow (t1,t2) => (toString t1) ^ " -> " ^ (toString t2)
    val toString = toString
  end

  structure RelSpec =
  struct
    structure TypeSpec =
    struct
      datatype t = T of Var.t * RefinementType.t
      val toString = fn T(var,refty) =>
        (Var.toString var) ^ " : "  ^ 
          (RefinementType.toString refty)
    end
    datatype t = T of StructuralRelation.t list * TypeSpec.t list
    val toString = fn T(reldecs,typespecs) =>
      let val srs = List.fold (reldecs, "", fn (reldec,acc) =>
            (StructuralRelation.toString reldec) ^ ";\n" ^ acc)
          val tss = List.fold (typespecs, "", fn (typespec,acc) =>
            (TypeSpec.toString typespec) ^ ";\n" ^acc)
      in
        srs^tss
      end

    fun layout spec = Layout.str (toString spec)
    val layout = layout
  end 
end

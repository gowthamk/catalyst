functor ANormalCoreML (S: A_NORMAL_CORE_ML_STRUCTS): A_NORMAL_CORE_ML = 
struct

open S

structure Field = Record.Field

fun maybeConstrain (x, t) =
   let
      open Layout
   in
      if !Control.showTypes
         then seq [x, str ": ", Type.layout t]
      else x
   end

fun layoutTargs (ts: Type.t vector) =
   let
      open Layout
   in
      if !Control.showTypes
         andalso 0 < Vector.length ts
         then list (Vector.toListMap (ts, Type.layout))
      else empty
   end

structure Value =
        struct
          datatype atom =
              Const of Const.t
            | Var of Var.t

          datatype t = 
              Atom of atom
            | Tuple of atom vector
            | Record of atom Record.t

          fun layout v = 
            let
              open Layout
            in
              case v of
                Atom (Const c) => Const.layout c
              | Atom (Var v) => Var.layout v
              | Tuple av => seq (Vector.toListMap (av, fn v => layout (Atom v)))
              | Record r => record (Vector.toListMap (Record.toVector r, fn (f, p) =>
                             (Field.toString f, layout (Atom p))))
            end
        end

structure Pat =
   struct
      datatype t = T of {node: node,
                         ty: Type.t}
      and node =
         Con of {arg: Value.t option,
                 con: Con.t,
                 targs: Type.t vector}
       | Layered of Var.t * Value.t
       | List of Value.atom vector
       | Value of Value.t
       | Wild

      local
         fun make f (T r) = f r
      in
         val dest = make (fn {node, ty} => (node, ty))
         val node = make #node
         val ty = make #ty
      end

      fun make (n, t) = T {node = n, ty = t}

      fun layout p =
         let
            val t = ty p
            open Layout
         in
            case node p of
               Con {arg, con, targs} =>
                  seq [Con.layout con,
                       layoutTargs targs,
                       case arg of
                          NONE => empty
                        | SOME p => Value.layout p]
             | Layered (x, p) =>
                  seq [maybeConstrain (Var.layout x, t), str " as ", Value.layout p]
             | List ps => list (Vector.toListMap (ps, Value.layout o Value.Atom))
             | Value v => Value.layout v
             | Wild => str "_"
         end
  end

structure NoMatch =
   struct
      datatype t = Impossible | RaiseAgain | RaiseBind | RaiseMatch
   end

datatype noMatch = datatype NoMatch.t

datatype dec =
   Datatype of {cons: {arg: Type.t option,
                       con: Con.t} vector,
                tycon: Tycon.t,
                tyvars: Tyvar.t vector} vector
 | Exception of {arg: Type.t option,
                 con: Con.t}
 | Fun of {decs: {lambda: lambda,
                  var: Var.t} vector,
           tyvars: unit -> Tyvar.t vector}
 | Val of {rvbs: {lambda: lambda,
                  var: Var.t} vector,
           tyvars: unit -> Tyvar.t vector,
           vbs: {exp: exp,
                 lay: unit -> Layout.t,
                 nest: string list,
                 pat: Pat.t} vector}
and exp = Exp of {node: expNode,
                  ty: Type.t}
and expNode =
   App of (unit -> Var.t) * 
          (unit -> Type.t vector) * 
          Value.t
 | Case of {kind: string,
            lay: unit -> Layout.t,
            nest: string list,
            rules: {exp: exp,
                    lay: (unit -> Layout.t) option,
                    pat: Pat.t} vector,
            test: Value.t}
 | EnterLeave of Value.t * SourceInfo.t
 | Handle of {catch: Var.t * Type.t,
              handler: exp,
              try: exp}
 | Lambda of lambda
 | Let of dec vector * exp
 | List of Value.atom vector
 | PrimApp of {args: Value.t vector,
               prim: Type.t Prim.t,
               targs: Type.t vector}
 | Raise of Value.t
 | Seq of exp vector
 | Var of (unit -> Var.t) * (unit -> Type.t vector)
 | Value of Value.t
and lambda = Lam of {arg: Var.t,
                     argType: Type.t,
                     body: exp}

local
   open Layout
in
   fun layoutTyvars (ts: Tyvar.t vector) =
      case Vector.length ts of
         0 => empty
       | 1 => seq [str " ", Tyvar.layout (Vector.sub (ts, 0))]
       | _ => seq [str " ", tuple (Vector.toListMap (ts, Tyvar.layout))]

   fun layoutConArg {arg, con} =
      seq [Con.layout con,
           case arg of
              NONE => empty
            | SOME t => seq [str " of ", Type.layout t]]

   fun layoutDec d =
      case d of
         Datatype v =>
            seq [str "datatype",
                 align
                 (Vector.toListMap
                  (v, fn {cons, tycon, tyvars} =>
                   seq [layoutTyvars tyvars,
                        str " ", Tycon.layout tycon, str " = ",
                        align
                        (separateLeft (Vector.toListMap (cons, layoutConArg),
                                       "| "))]))]
       | Exception ca =>
            seq [str "exception ", layoutConArg ca]
       | Fun {decs, tyvars, ...} => layoutFuns (tyvars, decs)
       | Val {rvbs, tyvars, vbs, ...} =>
            align [layoutFuns (tyvars, rvbs),
                   align (Vector.toListMap
                          (vbs, fn {exp, pat, ...} =>
                           seq [str "val",
                                mayAlign [seq [layoutTyvars (tyvars ()),
                                               str " ", Pat.layout pat,
                                               str " ="],
                                          layoutExp exp]]))]
   and layoutExp (Exp {node, ...}) =
      case node of
         App (f, targs, arg) => 
          let
            val targs = targs()
            val f = f ()
            val tylayt = if Vector.isEmpty targs
              then empty
              else seq [Vector.layout Type.layout targs, str " "]
          in
            paren (seq [Var.layout f, str " ", tylayt, Value.layout arg])
          end 
       | Case {rules, test, ...} =>
            Pretty.casee {default = NONE,
                          rules = Vector.map (rules, fn {exp, pat, ...} =>
                                              (Pat.layout pat, layoutExp exp)),
                          test = Value.layout test}
       | EnterLeave (v, si) =>
            seq [str "EnterLeave ",
                 tuple [Value.layout v, SourceInfo.layout si]]
       | Handle {catch, handler, try} =>
            Pretty.handlee {catch = Var.layout (#1 catch),
                            handler = layoutExp handler,
                            try = layoutExp try}
       | Lambda l => layoutLambda l
       | Let (ds, e) =>
            Pretty.lett (align (Vector.toListMap (ds, layoutDec)),
                         layoutExp e)
       | List es => list (Vector.toListMap (es, Value.layout o Value.Atom))
       | PrimApp {args, prim, targs} =>
            Pretty.primApp {args = Vector.map (args, Value.layout),
                            prim = Prim.layout prim,
                            targs = Vector.map (targs, Type.layout)}
       | Raise v => Pretty.raisee (Value.layout v)
       | Seq es => Pretty.seq (Vector.map (es, layoutExp))
       | Var (var, targs) => 
            if !Control.showTypes
               then let 
                       open Layout
                       val targs = targs ()
                    in
                       if Vector.isEmpty targs
                          then Var.layout (var ())
                       else seq [Var.layout (var ()), str " ",
                                 Vector.layout Type.layout targs]
                    end
            else Var.layout (var ())
   and layoutFuns (tyvars, decs)  =
      if 0 = Vector.length decs
         then empty
      else
         align [seq [str "val rec", layoutTyvars (tyvars ())],
                indent (align (Vector.toListMap
                               (decs, fn {lambda as Lam {argType, body = Exp {ty = bodyType, ...}, ...}, var} =>
                                align [seq [maybeConstrain (Var.layout var, Type.arrow (argType, bodyType)), str " = "],
                                       indent (layoutLambda lambda, 3)])),
                        3)]
   and layoutLambda (Lam {arg, argType, body, ...}) =
      paren (align [seq [str "fn ", 
                         maybeConstrain (Var.layout arg, argType),
                         str " =>"],
                    layoutExp body])

   fun layoutExpWithType (exp as Exp {ty, ...}) =
      let
         val node = layoutExp exp
      in
         if !Control.showTypes
            then seq [node, str " : ", Type.layout ty]
         else node
      end
end

structure Lambda =
   struct
      datatype t = datatype lambda

      val make = Lam

      fun dest (Lam r) = r

      val bogus = make {arg = Var.newNoname (),
                        argType = Type.unit,
                        body = Exp {node = Seq (Vector.new0 ()),
                                    ty = Type.unit} }
   end

structure Exp =
   struct
      type dec = dec
      type lambda = lambda
      datatype t = datatype exp
      datatype node = datatype expNode

      datatype noMatch = datatype noMatch

      val layout = layoutExp
      val layoutWithType = layoutExpWithType

      local
         fun make f (Exp r) = f r
      in
         val dest = make (fn {node, ty} => (node, ty))
         val node = make #node
         val ty = make #ty
      end

      fun make (n, t) = Exp {node = n,
                             ty = t}

      fun var (x: Var.t, ty: Type.t): t =
         make (Var (fn () => x, fn () => Vector.new0 ()), ty)
   end

structure Dec =
   struct
      datatype t = datatype dec

      val layout = layoutDec
   end

structure Program =
   struct
      datatype t = T of {decs: Dec.t vector}

      fun layouts (T {decs, ...}, output') =
         let
            open Layout
            (* Layout includes an output function, so we need to rebind output
             * to the one above.
             *)
            val output = output'
         in
            output (Layout.str "\n\nDecs:")
            ; Vector.foreach (decs, output o Dec.layout)
         end
   end

end

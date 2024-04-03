signature ENV =
sig
   type 'a env
   val empty: 'a env
   val insert: 'a env -> string * 'a -> 'a env
   val update: 'a env -> string -> 'a -> 'a env
   val lookup: 'a env -> string -> 'a option
   val concat: 'a env -> 'a env -> 'a env
end

signature RSL =
sig
   exception EvalError of string
   structure Env : ENV

   datatype binop = ADD | SUB | MULT | DIV | MOD
   datatype logop = EQ | LT | LE | NEQ
   datatype rslVal =
        IntV of int
      | BoolV of bool
      | PairV of rslVal * rslVal
      | UnitV
      | ClosV of { env: rslVal Env.env, f: rslExp }
      | PromV of rslVal option ref  (* Promise value, for use with letrec *)
      | ErrV of string


   and rslExp =
        Int of int
      | Bool of bool
      | Var of string
      | Binop of binop * rslExp * rslExp
      | Logop of logop * rslExp * rslExp
      | If of rslExp * rslExp * rslExp
      | Func of string * rslExp
      | Call of rslExp * rslExp
      | Letrec of (string * rslExp) list * rslExp
      | Pair of rslExp * rslExp
      | IsAPair of rslExp
      | Fst of rslExp
      | Snd of rslExp
      | Unit

   val show: rslVal -> string
   val typ: rslVal -> string
   val evalEnv: rslVal Env.env -> rslExp -> rslVal
   val eval: rslExp -> rslVal
end
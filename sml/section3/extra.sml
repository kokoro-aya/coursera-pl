
fun makeCounter i : unit -> int = fn () => i

fun makeMultiCounter : unit -> string -> int = fn () => fn s => 1

fun makeMultiCounter2 : unit -> string -> int = fn () => fn s => 1

fun gen (f: 'a -> 'a) (init: 'a) : unit -> 'a = false

fun once (f: unit -> 'a): unit -> 'a = false

fun only (i: int) (f: unit -> 'a): unit -> 'a = false

fun cache (f: 'a -> 'b): 'a -> 'b = false

fun cache2 (i: int) (f: 'a -> 'b): 'a -> 'b = false

fun throttle (i: int) (f: 'a list -> unit): 'a -> unit = false

fun throttle2 (i: int) (f: 'a -> unit): 'a -> unit = false

fun delayed (f: 'a -> 'b) (v: 'a): 'a -> 'b = false

fun befor (v: ('a * 'b)): 'a = false



(* A simple hash table *)

type 'a hashVector = 'a list ref vector

type ('a, 'b) hashTable = { hash: 'a -> int, eq: 'a * 'a -> bool, size: int, vec: ('a * 'b) hashVector }

val makeEmpty: ('a -> int, 'a * 'a -> bool, int) -> ('a, 'b) = false

val lookup_list: ('a * 'a -> bool) * ('a * 'b) list -> 'a -> 'b option = false
val insert_list: ('a * 'a -> bool) * ('a * 'b) list -> ('a, 'b) -> unit = false
val remove_list: ('a * 'a -> bool) * ('a * 'b) list -> 'a -> unit = false

val lookup: ('a, 'b) hashTable -> 'a -> 'b option = false
val insert: ('a, 'b) hashTable -> 'a * 'b -> unit = false
val remove: ('a, 'b) hashTable -> 'a -> unit = false



(* A simple calculator *)

datatype Exp = Add of Exp * Exp
             | Sub of Exp * Exp
             | Mult of Exp * Exp
             | Const of int
             | Var of string
             | Compound of Stm list * Exp
and      Stm = Assign of string * Exp
             | Print of Exp

exception UnboundVar of string

type memory = (string * int) list

val save: memory -> string * int -> memory = false
val load: memory -> string -> int = false

val evalExp: memory -> Exp -> int = false
val evalStm: memory -> Stm -> memory = false

fun evalExp mem e =
   let val ev = evalExp mem
   in
      case e of
        Add (e1, e2)       => ...
      | Sub (e1, e2)       => ...
      | Mult (e1, e2)      => ...
      | Const i            => ...
      | Var s              => ...
      | Compound (stms, e) => ...
   end
and evalStm mem stm =
   case stm of
     Assign (s, e)      => ...
   | Print (Var s)      => ...
   | Print e            => ...
val eval = evalExp []    (* Shortcut for evaluation with empty memory. No need to change. *)



(* Simple stack-based calculator *)

exception Empty
type stack = int list
val empty: stack = false
val push: stack -> int -> stack = false
val pop: stack -> (i, stack) = false

datatype oper = Push of int | Pop | Add | Sub | Mult | Neg | Print

val evalOp: stack -> oper -> stack = false
val eval: oper list -> unit = false

fun evalOp stk o =
   case (o, stk) of
     (Push i, _)          => ...
   | (Pop, _::stk')       => ...
   | (Add, i1::i2::stk')  => ...
   | (Sub, i1::i2::stk')  => ...
   | (Mult, i1::i2::stk') => ...
   | (Neg, i1::stk')      => ...
   | (Print, i1::_)       => ...
   | _                    => ...
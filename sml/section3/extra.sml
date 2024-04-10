
fun makeCounter i : unit -> int = 
  let 
    val cnt = ref i
    fun f () =
      let val last = !cnt
      in
        cnt := last + 1; last
      end
    in
      f
  end  
  

fun makeMultiCounter () : string -> int = 
    let 
    val dict: (string * int) list ref = ref []
    fun f () (s: string) =
      let val lastState: (string * int) list = !dict
      in
        let val nextNum: int = case (List.find (fn (label, _) => label = s) lastState) of
            NONE => 1
          | SOME (_,  num) => num + 1
        in
          dict := ((s, nextNum) :: lastState);
          nextNum
        end
      end
    in
      f ()
  end  



fun gen (f: 'a -> 'a) (s: 'a) : unit -> 'a =
  let
    val storage = ref s
    fun func () =
     let val _ = storage := f (!storage) in (!storage) end
  in
    func 
  end

 fun makeCounterByGen i = gen (fn x => x + 1) (i-1)


fun once (f: unit -> 'a): unit -> 'a = 
  let
    val cachedRes: 'a option ref = ref NONE
    fun func () =
      (case (!cachedRes) of
          NONE => let
                    val res = f () 
                    val _ = cachedRes := SOME res in res end
        | SOME x => x)
    in
      func
  end


fun only (sz: int) (f: unit -> 'a): unit -> 'a = 
  let
    val cachedRes: 'a list ref = ref []
    fun func () =
      if (List.length (!cachedRes)) < sz then
        let 
          val res = f ()
          val _ = cachedRes := res :: (!cachedRes)
        in
          res
        end
      else 
        let
          val lastItem = List.last (!cachedRes) 
          val _ = cachedRes := lastItem :: List.take (!cachedRes, sz - 1) 
        in
          lastItem
        end
  in
    func 
  end


(* ''a for equality *)
fun cache (f: ''a -> 'b): ''a -> 'b = 
  let 
    val dict: (''a * 'b) list ref = ref []
    fun func (actual: ''a) =
      let val lastState: (''a * 'b) list = !dict
      in
        case (List.find (fn (arg, _) => arg = actual) lastState) of
            NONE => 
              let val res: 'b = f actual in
                dict := (actual, res) :: lastState; res
              end
          | SOME (_,  value) => value 
      end
    in
      func 
  end  


fun cacheWithSize (n: int) (f: ''a -> 'b): ''a -> 'b = 
  let 
    val dict: (''a * 'b) list ref = ref []
    fun func (actual: ''a) =
      let val lastState: (''a * 'b) list = !dict
      in
        case (List.find (fn (arg, _) => arg = actual) lastState) of
            NONE => 
              let val res: 'b = f actual in
                dict := (actual, res) :: (if (List.length lastState) < n then lastState else List.take (lastState, List.length lastState - 1))
                ; res
              end
          | SOME (_,  value) => value
      end
    in
      func
  end  


fun throttle (n: int) (f: 'a list -> unit) : 'a -> unit =
  let
    val cached: 'a list ref = ref []
    fun func (el: 'a) =
      let val _ = cached := el :: (!cached) 
      in
        if List.length (!cached) = n then
          let 
            val res = f (!cached)
          in
            cached := []; res
          end
        else
          ()
      end
  in
    func
  end

(* To test throttle function *)
fun printList lst = case lst of
  [] => () | x :: xs => let val _ = print (Int.toString x) in printList xs end

val someFuncThrottle = throttle 3 printList


fun throttle2 (n: int) (f: 'a -> unit) : 'a -> unit =
  let
    val cached: 'a option ref = ref NONE
    val count = ref 0
    fun func (el: 'a) =
      if (!count) < n then
        let val _ = case (!cached) of
            SOME _ => ()
          | NONE => cached := SOME el 
        in 
          count := (!count) + 1
        end
      else
        let val res = case (!cached) of
            SOME x => f x | NONE => ()
        in
          cached := SOME el ; res
        end
  in
    func
  end

fun delayed (f: 'a -> 'b) (v: 'a): 'a -> 'b =
  let 
    val lastArg = ref v
    fun func (arg: 'a) =
      let val res = f (!lastArg) in
      lastArg := arg; res
      end
    in
      func
    end


fun befor (left: 'a) (right: 'b): 'a =
  let 
    val evRes = left
    val _ = right
  in
    evRes
  end


(* A simple hash table *)

(*
type 'a hashVector = 'a list ref vector

type ('a, 'b) hashTable = { hash: 'a -> int, eq: 'a * 'a -> bool, size: int, vec: ('a * 'b) hashVector }

val makeEmpty: ('a -> int, 'a * 'a -> bool, int) -> ('a, 'b) = false

val lookup_list: ('a * 'a -> bool) * ('a * 'b) list -> 'a -> 'b option = false
val insert_list: ('a * 'a -> bool) * ('a * 'b) list -> ('a, 'b) -> unit = false
val remove_list: ('a * 'a -> bool) * ('a * 'b) list -> 'a -> unit = false

val lookup: ('a, 'b) hashTable -> 'a -> 'b option = false
val insert: ('a, 'b) hashTable -> 'a * 'b -> unit = false
val remove: ('a, 'b) hashTable -> 'a -> unit = false

*)

(* A simple calculator *)

(*

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


*)

(* Simple stack-based calculator *)

exception Empty
type stack = int list
fun stack_empty (): stack = []

fun stack_push stack v: stack = v :: stack

fun stack_pop (s: stack): int * stack = (List.hd s, List.tl s)

datatype oper = Push of int | Pop | Add | Sub | Mult | Neg | Print


fun evalOp (stk: stack) (oper: oper): stack =
   case (oper, stk) of
     (Push i, _)          => stack_push stk i
   | (Pop, _::stk')       => stk'
   | (Add, i1::i2::stk')  => stack_push stk' (i1 + i2)
   | (Sub, i1::i2::stk')  => stack_push stk' (i1 - i2)
   | (Mult, i1::i2::stk') => stack_push stk' (i1 * i2)
   | (Neg, i1::stk')      => stack_push stk' (~i1)
   | (Print, i1::_)       => 
        let val _ = print (Int.toString i1) in stk end
   | _                    => 
        let val _ = print "Error found" in stk end

fun eval (opers: oper list): unit =
  let 
    val _ = List.foldl (fn (oper, st) => evalOp st oper) (stack_empty ()) opers 
  in 
    ()
  end

val a = [Push 3, Push 4, Add, Print]

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

fun createListOfInits a n = 
  if n <= 0 then [] else a :: createListOfInits a (n - 1)

fun initEmptyLists n = createListOfInits (ref []) n


type 'a hashVector = 'a list ref vector

type ('a, 'b) hashTable = { hash: 'a -> int, eq: 'a * 'a -> bool, size: int, vec: ('a * 'b) hashVector }

fun makeEmpty (hashFunc: 'a -> int, eqFunc: 'a * 'a -> bool, size: int): ('a, 'b) hashTable = 
  { hash=hashFunc, eq=eqFunc, size=size, vec = Vector.fromList (initEmptyLists size) }

fun lookup_list (eqFunc: 'a * 'a -> bool) (lst: ('a * 'b) list) (key: 'a): 'b option = case lst of
        [] => NONE
      | (x, v) :: xs => if eqFunc (x, key) then SOME v else lookup_list eqFunc xs key

fun insert_list (eqFunc: 'a * 'a -> bool) (lst: ('a * 'b) list) ((k, v1): 'a * 'b): ('a * 'b) list = case lst of
        [] => (k, v1) :: []
      | (x, v) :: xs => if eqFunc (x, k) then (x, v1) :: xs else (x, v) :: insert_list eqFunc xs (k, v1)

fun remove_list (eqFunc: 'a * 'a -> bool) (lst: ('a * 'b) list) (key: 'a): ('a * 'b) list = case lst of
        [] => []
      | (x, v) :: xs => if eqFunc (x, key) then xs else (x, v) :: remove_list eqFunc xs key

fun lookup ({ hash, eq, size, vec }: ('a, 'b) hashTable) (k: 'a): 'b option =
        let val idx = hash k 
            val inner = Vector.sub (vec, idx)
            val res = lookup_list eq (!inner) k
        in
          res
        end

fun insert ({ hash, eq, size, vec }: ('a, 'b) hashTable) ((k, v): ('a * 'b)): unit = 
        let val idx = hash k
            val inner = Vector.sub (vec, idx)
            val updated = insert_list eq (!inner) (k, v)
        in
          Vector.update (vec, idx, (ref updated)) ; ()
        end

fun remove ({ hash, eq, size, vec }: ('a, 'b) hashTable) (k: 'a): unit =
        let val idx = hash k 
            val inner = Vector.sub (vec, idx)
            val updated = remove_list eq (!inner) k
        in
          Vector.update (vec, idx, (ref updated)) ; ()
        end


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

val empty_memory: memory = []

fun save (mem: memory) (var: (string * int)): memory =
  var :: mem

fun load (mem: memory) (name: string): int = case mem of
      [] => raise UnboundVar name
    | (label, value) :: xs => if name = label then value else load xs name


fun evalExp (mem: memory) (e: Exp): int =
   let val ev = evalExp mem
   in
      case e of
        Add (e1, e2)       => evalExp mem e1 + evalExp mem e2
      | Sub (e1, e2)       => evalExp mem e1 - evalExp mem e2
      | Mult (e1, e2)      => evalExp mem e1 * evalExp mem e2
      | Const i            => i
      | Var s              => load mem s
      | Compound (stms, ex) => 
          let val env = List.foldl (fn (stm, m1) => evalStm m1 stm) empty_memory stms
          in
            evalExp env ex
          end
   end
and evalStm (mem: memory) (stm: Stm): memory =
   case stm of
     Assign (s, e)      => save mem (s, (evalExp mem e))
   | Print (Var s)      => 
      let val _ = print (Int.toString (load mem s)) in mem end
   | Print e            => 
      let val _ = print (Int.toString (evalExp mem e)) in mem end


val eval = evalExp []    (* Shortcut for evaluation with empty memory. No need to change. *)



(* Simple stack-based calculator *)

exception Empty
type stack = int list
fun stack_empty (): stack = []

fun stack_push stack v: stack = v :: stack

fun stack_pop (s: stack): int * stack = (List.hd s, List.tl s)

datatype oper = PUSH of int | POP | ADD | SUB | MULT | NEG | PRINT


fun evalOp (stk: stack) (oper: oper): stack =
   case (oper, stk) of
     (PUSH i, _)          => stack_push stk i
   | (POP, _::stk')       => stk'
   | (ADD, i1::i2::stk')  => stack_push stk' (i1 + i2)
   | (SUB, i1::i2::stk')  => stack_push stk' (i1 - i2)
   | (MULT, i1::i2::stk') => stack_push stk' (i1 * i2)
   | (NEG, i1::stk')      => stack_push stk' (~i1)
   | (PRINT, i1::_)       => 
        let val _ = print (Int.toString i1) in stk end
   | _                    => 
        let val _ = print "Error found" in stk end

fun eval1 (opers: oper list): unit =
  let 
    val _ = List.foldl (fn (oper, st) => evalOp st oper) (stack_empty ()) opers 
  in 
    ()
  end

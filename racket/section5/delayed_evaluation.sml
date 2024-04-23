
signature LAZY =
sig
   type 'a lazy
   val delay: (unit -> 'a) -> 'a lazy
   val force: 'a lazy -> 'a
end


structure Lazy :> LAZY =
struct
   type 'a thunk = unit -> 'a
   datatype 'a promise = VALUE of 'a | THUNK of 'a thunk
   type 'a lazy = 'a promise ref

   fun delay (f: unit -> 'a): 'a lazy    = ref (THUNK f)
   fun force (aRef: 'a lazy): 'a = 
      let 
         val tk: 'a promise = !aRef
      in
         case tk of
             VALUE x => x
           | THUNK f => 
              let val res: 'a = f ()
                  val _ = aRef := VALUE res
              in
                res
              end
      end
end



signature STREAM =
sig
   exception EndOfStream
   type 'a stream

   val build: ('b -> ('a * 'b) option) -> 'b -> 'a stream

   val next: 'a stream -> 'a * 'a stream
   val safe_next : 'a stream -> ('a * 'a stream) option
   
end


structure Stream :> STREAM =
struct
   exception EndOfStream
   datatype 'a stream = Stream of unit -> 'a * 'a stream

   fun build (f: 'b -> ('a * 'b) option) (init: 'b): 'a stream =
      Stream (fn () => 
         let val res = f init in
            case res of
              SOME (a, b) => (a, build f b)
            | NONE => raise EndOfStream
         end
      )

   fun next ((Stream th): 'a stream): 'a * 'a stream = th ()
   fun safe_next (s: 'a stream): ('a * 'a stream) option = SOME (next s) handle EndOfStream => NONE

end

(* We have to put the generate logic into Stream because building it in exterior will result in unresolved `Stream` name *)
fun generate (f: 'b -> ('a * 'b) option) (init: 'b) = Stream.build f init
fun generate_inf (f: 'b -> 'a * 'b) (init: 'b) = generate (fn b => SOME (f b)) init
fun generate_simple (f: 'a -> 'a) (init: 'a) = generate_inf (fn a => (a, f a)) init

fun take (i: int) (st: 'a Stream.stream): 'a list =
  if i <= 0 then []
  else let val (cur, nx) = Stream.next st
    in
      cur :: take (i-1) nx
    end

fun safe_take (i: int) (st: 'a Stream.stream): 'a list =
  if i <= 0 then []
  else let val optNext = Stream.safe_next st
    in
      case optNext of 
          SOME (cur, nx) =>
            cur :: safe_take (i-1) nx
        | NONE => []
    end



fun seq (begin: int) (endl: int) = 
   generate (fn x => if x > endl then NONE else SOME (x, x + 1)) begin

fun const (v: 'a) = 
   generate_inf (fn _ => (v, v)) v

fun fromList (lst: 'a list) = 
   generate (fn xs => case xs of [] => NONE | x :: xss => SOME (x, xss)) lst

fun replay (init_st: 'a Stream.stream) = 
   generate (fn last =>
         case Stream.safe_next last of
              SOME x => SOME x
            | NONE => Stream.safe_next init_st
   ) init_st
fun cycleList (xs: 'a list) = 
   replay (fromList xs)

fun map (f: 'a -> 'b) (st: 'a Stream.stream) = 
   generate (fn last => 
      case Stream.safe_next last of
           SOME (x, nx) => SOME (f x, nx)
         | NONE => NONE
   ) st

fun until (f: 'a -> bool) (st: 'a Stream.stream) = 
   generate (fn last => 
      case Stream.safe_next last of
           SOME (x, nx) => 
            if f x then NONE else SOME (x, nx)
         | NONE => NONE
   ) st

fun filter (f: 'a -> bool) (init_st: 'a Stream.stream) = 
   generate (fn last => 
      let fun loop st = 
         case Stream.safe_next st of
              SOME (x, nx) => 
                 if f x then SOME (x, nx) else loop nx
            | NONE => NONE
      in
         loop last
      end
   ) init_st

fun zip (sa: 'a Stream. stream) (sb: 'b Stream.stream) = 
   generate (fn (a, b) => 
      case (Stream.safe_next a, Stream.safe_next b) of
          (SOME (x, nx), SOME (y, ny)) => SOME ((x, y), (nx, ny))
         | _ => NONE
   ) (sa, sb)

fun from_f (f: int -> 'a): 'a Stream.stream = 
   map f (generate_inf (fn x => (x, x+1)) 1)

fun iterate (st: 'a Stream.stream): unit -> 'a =
   let
    val stref = ref st
    fun iterate_inner () = 
      let 
         val (x, nx) = Stream.next (!stref)
         val _ = stref := nx
      in
         x
      end
   in 
      iterate_inner 
   end

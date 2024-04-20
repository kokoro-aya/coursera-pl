
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



(*

val seq: int -> int -> int stream  (* from / step *)
val const: 'a -> 'a stream
val fromList: 'a list -> 'a stream  (* EOS once list ends *)
val replay: 'a stream -> 'a stream  (* once stream hits EOS, "restart" it *)
val cycleList: 'a list -> 'a stream (* start afresh once list ends *)
val map: ('a -> 'b) -> 'a stream -> 'b stream
(* until moves along until a v with (p v) true. May not terminate. *)
val until: ('a -> bool) -> 'a stream -> 'a * 'a stream
val filter: ('a -> bool) -> 'a stream -> 'a stream
val zip: 'a stream * 'b stream -> ('a * 'b) stream (* EOS when one of the streams does *)
val pack: int -> 'a stream -> 'a list stream (* Will discard shorter list on EOS *)
val interleave: 'a stream list -> 'a stream (* If any EOS, discard it and keep going *)
val cumulative: ('b * 'a -> 'b) -> 'b -> 'a stream -> 'b stream
val from_f: (int -> 'a) -> 'a stream    (* f 1, f 2, f 3, ... *)
val iterate: 'a stream -> unit -> 'a

*)
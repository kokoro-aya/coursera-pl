
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

   fun delay f    = ...
   fun force aRef = ...
end



signature STREAM =
sig
   exception EndOfStream
   type 'a stream
   val next: 'a stream -> 'a * 'a stream
   val safe_next : 'a stream -> ('a * 'a stream) option
   ...
end


structure Stream :> STREAM =
struct
   exception EndOfStream
   datatype 'a stream = Stream of unit -> 'a * 'a stream

   fun next (Stream th) = ...
   fun safe_next s = ...



val generate: ('b -> ('a * 'b) option) -> 'b -> 'a stream
val generate_inf: ('b -> 'a * 'b) -> 'b -> 'a stream
val generate_simple: ('a -> 'a) -> 'a -> 'a stream


val take: int -> 'a stream -> 'a list
val safe_take: int -> 'a stream -> 'a list


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
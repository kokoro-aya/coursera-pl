signature ENV =
sig
   type 'a env
   val empty: 'a env
   val insert: 'a env -> string * 'a -> 'a env
   val lookup: 'a env -> string -> 'a option
   val concat: 'a env -> 'a env -> 'a env
end
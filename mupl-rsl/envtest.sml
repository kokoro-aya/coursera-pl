use "signatures.sml";

(* exception TestFailedException of string means registered, not raised *)
(* if no uncaught exception happened after all file loaded, then the code should pass all tests *)
exception TestFailedException of string

structure Env :> ENV = struct
    type 'a env = (string *'a) list
    val empty = nil

    fun insert env kv = kv :: env

    fun lookup env k = case env of
        [] => NONE
      | (a, b) :: xs => if a = k then SOME b else lookup xs k

    fun concat xs ys = xs @ ys

    val testemptyenv = 
        if null empty then () else raise TestFailedException "testemptyenv"

    val testinsertenv = 
        if insert empty ("foo", 12) = ("foo", 12) :: nil then () else raise TestFailedException "testinsertenv"

    val testlookupenv1 = 
        if lookup [("a", 12), ("b", 49), ("c", 22)] "b" = SOME 49 then () else raise TestFailedException "testlookupenv1"

    val testlookupenv2 = 
        if lookup [("a", 12), ("b", 49), ("c", 22)] "d" = NONE then () else raise TestFailedException "testlookupenv2"
end
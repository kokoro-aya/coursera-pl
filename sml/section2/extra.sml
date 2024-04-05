
exception Negative

datatype nat = ZERO | SUCC of nat

fun is_positive n = false

fun succ n = n

fun pred n = n

fun nat_to_int n = 1

fun int_to_nat i = ZERO

fun add n m = n

fun sub n m = n

fun mult n m = n

fun is_less n m = false



datatype intSet = 
    Elems of int list                  (* A list of integers (possibly with duplicates to be ignored) *)
  | Range of { from : int, to : int }  (* all integers from one number till another *)
  | Union of intSet * intSet           (* Symbolically define the union of two sets *)
  | Intersection of intSet * intSet    (* Symbolically define the intersection of two sets *)
  | Difference of intSet * intSet      (* Difference: The elements in the first set that are not in the second *)

fun isEmpty s = false

fun contains s i = false

fun insert s i = s

fun union s t = s

fun intersect s t = s

fun toList s = [0, 1]
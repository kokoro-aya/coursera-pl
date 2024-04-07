
exception Negative

datatype nat = ZERO | SUCC of nat

fun is_positive n = case n of ZERO => false | SUCC _ => true

fun succ n = SUCC (n)

fun pred n = case n of ZERO => raise Negative | SUCC x => x

fun nat_to_int n = case n of ZERO => 0 | SUCC x => 1 + nat_to_int x

fun int_to_nat i = if i < 0 then raise Negative else if i = 0 then ZERO else SUCC (int_to_nat (i - 1))

fun add n m = case n of ZERO => m | SUCC x => succ (add x m)

fun sub n m = case m of
        ZERO => n
      | SUCC x => sub (pred n) (pred m)

fun mult n m = case n of
        ZERO => ZERO
      | SUCC ZERO => m
      | SUCC x => add m (mult (pred n) m)

fun is_less n m = case (sub m n) of
        ZERO => false
      | SUCC x => true
      handle Negative => false


datatype intSet = 
    Elems of int list                  (* A list of integers (possibly with duplicates to be ignored) *)
  | Range of { from : int, to : int }  (* all integers from one number till another *)
  | Union of intSet * intSet           (* Symbolically define the union of two sets *)
  | Intersection of intSet * intSet    (* Symbolically define the intersection of two sets *)
  | Difference of intSet * intSet      (* Difference: The elements in the first set that are not in the second *)

fun isEmpty s = case s of
      Elems [] => true
    | Elems _ => false
    | Range { from, to } => if from <= to then false else true
    | Union (s1, s2) => isEmpty s1 andalso isEmpty s2
    | Intersection (s1, s2) =>isEmpty s1 orelse isEmpty s2
    | Difference (s1, s2) => isEmpty s1 (* wrong *)

fun contains s i = case s of
      Elems xs => List.exists (fn x => x = i) xs
    | Range { from, to } => i >= from andalso i <= to
    | Union (s1, s2) => contains s1 i orelse contains s2 i
    | Intersection (s1, s2) =>contains s1 i andalso contains s2 i
    | Difference (s1, s2) => contains s1 i andalso (not (contains s2 i))


fun range (start, stop, step) = 
  if start > stop then []
  else start :: range (start + step, stop, step)

fun insert s i = case s of
      Elems xs => Elems xs
    | Range { from, to } => 
        if i = from - 1 
        then Range { from=from-1, to=to }
        else if i = to + 1
        then Range { from=from, to=to+1 }
        else Elems (i :: range(from, to, 1))
    | Union (s1, s2) => Union (insert s1 i, s2)
    | Intersection (s1, s2) => Intersection (insert s1 i, insert s2 i)
    | Difference (s1, s2) => Difference (insert s1 i, s2)

fun union s t = Union (s, t)

fun intersect s t = Intersection (s, t)


fun insertAscendingUnduplicated e xs = case xs of
    [] => e :: []
  | x::ys => if e < x then e :: xs else if e = x then xs else x :: insertAscendingUnduplicated e ys

fun mergeTwoListsAscendingUnduplicated lista listb = case lista of
    [] => listb
  | x :: xs => mergeTwoListsAscendingUnduplicated xs (insertAscendingUnduplicated x listb)

fun includeOnlyElementsFromAnotherList lista listb = List.filter (fn x => List.exists (fn y => x = y) listb) lista

fun excludeElementsFromAnotherList lista listb = List.filter (fn x => not (List.exists (fn y => x = y) listb)) lista

fun toListSub s acc = case s of 
      Elems xs => mergeTwoListsAscendingUnduplicated xs []
    | Range { from, to } => range (from, to ,1)
    | Union (s1, s2) => 
        let 
          val lista = toListSub s1 []
          val listb = toListSub s2 [] in
            mergeTwoListsAscendingUnduplicated lista listb
          end
    | Intersection (s1, s2) =>
        let 
          val lista = toListSub s1 []
          val listb = toListSub s2 [] in
            includeOnlyElementsFromAnotherList lista listb
          end
    | Difference (s1, s2) => 
        let 
          val lista = toListSub s1 []
          val listb = toListSub s2 [] in
            excludeElementsFromAnotherList lista listb
          end


fun toList s = toListSub s []
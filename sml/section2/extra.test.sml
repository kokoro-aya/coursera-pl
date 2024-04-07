use "extra.sml";

val a = is_positive ZERO = false
val b = succ ZERO = SUCC ZERO
val c = pred (SUCC (SUCC ZERO)) = SUCC ZERO
val d = nat_to_int (SUCC (SUCC (SUCC ZERO))) = 3
val e = int_to_nat 2 = SUCC (SUCC ZERO)
val f = add (SUCC (SUCC ZERO)) (SUCC ZERO) = (SUCC (SUCC (SUCC ZERO)))
val g = sub (SUCC (SUCC (SUCC ZERO))) (SUCC (SUCC ZERO)) = (SUCC ZERO)
val h = mult (SUCC (SUCC ZERO)) (SUCC (SUCC (SUCC ZERO))) = int_to_nat 6
val i = is_less (SUCC (SUCC ZERO)) (SUCC (SUCC (SUCC ZERO))) = true
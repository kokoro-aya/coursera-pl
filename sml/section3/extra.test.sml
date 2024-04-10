use "extra.sml";

val f1 = gen (fn x => x * x) 2

val a1 = f1 () = 4

val a2 = f1 () = 16

val f2 = makeCounterByGen 2

val b1 = f2 () = 2

val b2 = f2 () = 3

val f3s = makeCounter 2
val f3 = only 2 f3s

val c1 = f3 () = 2
val c2 = f3 () = 3
val c3 = f3 () = 2
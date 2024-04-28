use "resil.typecheck.sml";

val exp1 = Resil.Int(5);
val exp2 = Resil.Bool(false);
val exp3 = Resil.Pair(Resil.Int(5), Resil.Int(6));

val cons1 = getConstraints Resil.Env.empty exp1;
val cons2 = getConstraints Resil.Env.empty exp2;
val cons3 = getConstraints Resil.Env.empty exp3;

val (t, (c1, c2) :: _) = cons3;
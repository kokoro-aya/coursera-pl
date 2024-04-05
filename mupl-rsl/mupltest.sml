use "mupl.sml";


val a = Mupl.eval (Mupl.IfGreater (Mupl.Int (3), Mupl.Int (4), Mupl.Int (3), Mupl.Int (2)))
val b = Mupl.eval (Mupl.MLet ("x", Mupl.Int(1), Mupl.Add(Mupl.Int(5), Mupl.Var("x"))))
val c = Mupl.eval (Mupl.Call (Mupl.Func(NONE, "x", Mupl.Add(Mupl.Var("x"), Mupl.Int(7))), Mupl.Int(1)))
val d = Mupl.eval (Mupl.Snd (Mupl.APair(Mupl.Int(1), Mupl.Int(2))))
val e = Mupl.eval (Mupl.IsAUnit(Mupl.Func(NONE, "x", Mupl.AUnit)))
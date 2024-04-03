use "resil.lang.sml";

val a = Resil.eval (Resil.If (Resil.Logop (Resil.LE, Resil.Int (3), Resil.Int (4)), Resil.Int (3), Resil.Int ( 2)))


val b = Resil.eval (Resil.Letrec (
    [
      ("x", Resil.Int(1)),
      ("y", Resil.Int(2)),
      ("z", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Var("y")))
    ],
    Resil.Var("z")))
    
(* val c = Resil.eval (Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(7)))) *)

val c = Resil.eval (Resil.Call (Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(7))), Resil.Int(1)))


val d = Resil.eval (Resil.Snd (Resil.Pair(Resil.Int(7), Resil.Int(2))))

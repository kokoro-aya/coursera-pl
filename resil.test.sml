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

(*
  fun f x = x + 1
  fun g x = x * 2
  
  f(g(3)) === 7
*)
val e = Resil.eval (Resil.Call (Resil.Func ("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1))), Resil.Call(Resil.Func ("x", Resil.Binop(Resil.MULT, Resil.Var("x"), Resil.Int(2))), Resil.Int(3))))


val e1 = Resil.eval (Resil.Letrec (
    [
      ("f", Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1)))),
      ("g", Resil.Func("x", Resil.Binop(Resil.MULT, Resil.Var("x"), Resil.Int(2)))),
      ("z", Resil.Int(3))
    ],
    Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("g"), Resil.Var("z")))))

(*
fun double f = f . f
let val x = 4
  in (double (+2)) x
*)

val f = Resil.eval(Resil.Letrec (
  [
    ("double", Resil.Func("f", Resil.Func("#x", Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("f"), Resil.Var("#x")))))),
    ("x", Resil.Int(4))
  ],

  Resil.Call(Resil.Call(Resil.Var("double"), Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Int(2), Resil.Var("x")))), Resil.Var("x"))))
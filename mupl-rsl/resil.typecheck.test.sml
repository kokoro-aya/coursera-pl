use "resil.typecheck.sml";

val exp1 = Resil.Int(5);
val exp2 = Resil.Bool(false);
val exp3 = Resil.Pair(Resil.Int(5), Resil.Int(6));
val exp4 = Resil.Pair(Resil.Pair(Resil.Str("foo"), Resil.Int(3)), Resil.Unit);

val bin1 = Resil.Binop(Resil.ADD, Resil.Int(3), Resil.Int(4));
val bin2 = Resil.Logop(Resil.LE, Resil.Int(3), Resil.Int(4));
val bin3 = Resil.Logop(Resil.EQ, Resil.Bool(true), Resil.Bool(false));

val pairfst = Resil.Fst(Resil.Pair(Resil.Pair(Resil.Str("foo"), Resil.Int(3)), Resil.Unit));
val pairsnd = Resil.Snd(Resil.Pair(Resil.Pair(Resil.Unit, Resil.Int(4)), Resil.Pair(Resil.Pair(Resil.Int(9), Resil.Str("bar")), Resil.Unit)));

(* isAPair *)

val if1 = Resil.If (Resil.Logop (Resil.LE, Resil.Int (3), Resil.Int (4)), Resil.Int (3), Resil.Int (2));

(* Functions *)

val func1 = Resil.Func("x", Resil.Int(1));
val func2 = Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Int(3), Resil.Var("x")));
val func3 = Resil.Func("x", Resil.Func("y", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Var("y"))));

(* TODO: Function that could be generic *)

val fnGen1 = Resil.Func("x", Resil.Var("x"));

(* Letrecs *)

val letrec1 = Resil.Letrec (
      [("x", Resil.Int(3))],
      Resil.Var("x")
    )

val letrec2 = Resil.Letrec (
      [
        ("f", Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1)))),
        ("x", Resil.Int(3)),
        ("y", Resil.Int(4))
      ],
      Resil.Var("f"))
    
val letrec3 = Resil.Letrec (
      [
        ("f", Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1)))),
        ("x", Resil.Int(3)),
        ("y", Resil.Int(4))
      ],
      Resil.Binop(Resil.ADD, Resil.Var("y"), Resil.Var("x")));

val letrec4 = Resil.Letrec (
    [
      ("f", Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1)))),
      ("g", Resil.Func("x", Resil.Binop(Resil.MULT, Resil.Var("x"), Resil.Int(2)))),
      ("z", Resil.Int(3))
    ],
    Resil.Pair(Resil.Var("f"), Resil.Var("g")));
    (* Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("g"), Resil.Var("z")))) *)

val letrec5 = Resil.Letrec (
    [
      ("f", Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1)))),
      ("g", Resil.Func("x", Resil.Binop(Resil.MULT, Resil.Var("x"), Resil.Int(2)))),
      ("z", Resil.Int(3))
    ],
    (Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("g"), Resil.Var("z")))));

(* Calls *)

val call1 = Resil.Call(Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1))), Resil.Call(Resil.Func("x", Resil.Binop(Resil.MULT, Resil.Var("x"), Resil.Int(2))), Resil.Int(3)));

(* Call dyns *)

(* Complex cases *)

val a = Resil.If (Resil.Logop (Resil.LE, Resil.Int (3), Resil.Int (4)), Resil.Int (3), Resil.Int ( 2))


val b = Resil.Letrec (
    [
      ("x", Resil.Int(1)),
      ("y", Resil.Int(2)),
      ("z", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Var("y")))
    ],
    Resil.Var("z"))

val c = Resil.Call (Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(7))), Resil.Int(1))

val d = Resil.Snd (Resil.Pair(Resil.Int(7), Resil.Int(2)))

val e = Resil.Call (Resil.Func ("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1))), Resil.Call(Resil.Func ("x", Resil.Binop(Resil.MULT, Resil.Var("x"), Resil.Int(2))), Resil.Int(3)))

val e1 = Resil.Letrec (
    [
      ("f", Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1)))),
      ("g", Resil.Func("x", Resil.Binop(Resil.MULT, Resil.Var("x"), Resil.Int(2)))),
      ("z", Resil.Int(3))
    ],
    Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("g"), Resil.Var("z"))))

(* TODO Fail, empty unification *)
val f = Resil.Letrec (
  [
    ("double", Resil.Func("f", Resil.Func("#x", Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("f"), Resil.Var("#x")))))),
    ("x", Resil.Int(4))
  ],

  Resil.Call(Resil.Call(Resil.Var("double"), Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Int(2), Resil.Var("x")))), Resil.Var("x")))

val f_ = Resil.Func("f", Resil.Func("#x", Resil.Call(Resil.Var("f"), Resil.Var("#x"))))
val f0 = Resil.Func("f", Resil.Func("#x", Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("f"), Resil.Var("#x")))))

val f1 = Resil.Letrec (
  [
    ("double", Resil.Func("f", Resil.Func("#x", Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("f"), Resil.Var("#x")))))),
    ("x", Resil.Int(4)),
    ("y", Resil.Int(5))
  ],
  Resil.Var("double"));

val fx = Resil.Letrec (
  [
    ("x", Resil.Int(4))
  ],
  Resil.Var("x"));

val f2 = Resil.Letrec (
  [
    ("double", Resil.Func("f", Resil.Func("#x", Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("f"), Resil.Var("#x")))))),
    ("x", Resil.Int(4))
  ],
  Resil.Var("x"));

val f3= Resil.Func("f", Resil.Func("#x", Resil.Call(Resil.Var("f"), Resil.Var("#x"))));

val g = Resil.Letrec(
    [
      ("y", Resil.Int(3)),
      ("f", Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("y"), Resil.Var("x"))))
    ],
    Resil.Call(Resil.Var("f"), Resil.Int(4)))


val g1 = Resil.Letrec(
    [
      ("x", Resil.Int(3)),
      ("y", Resil.Int(4)),
      ("z", Resil.Int(5)),
      ("f", Resil.Func("x", 
              Resil.Func("y", 
                Resil.Func("z", 
                  Resil.Letrec(
                    [("w", Resil.Int(6))],
                    Resil.Binop(Resil.ADD, Resil.Var("w"),
                    Resil.Binop(Resil.ADD, Resil.Var("x"),
                    Resil.Binop(Resil.ADD, Resil.Var("y"), Resil.Var("z"))))))))
                  )
                  
    ],
    Resil.Call(Resil.Call(Resil.Call(Resil.Var("f"), Resil.Var("x")), Resil.Var("y")), Resil.Var("z")))

(* Fail unknown g *)
val g3 = Resil.Letrec(
  [
    ("f", Resil.Func("x", Resil.Call(Resil.Var("g"), Resil.Unit))),
    ("g", Resil.Func("x", Resil.Call(Resil.Var("h"), Resil.Unit))),
    ("h", Resil.Func("x", Resil.Int(2)))
  ],
  Resil.Call(Resil.Var("f"), Resil.Unit))

(* val (ty, ccs) = getConstraints Resil.Env.empty letrec3;; *)

(* val (c1, c2) :: (c3, c4) :: (c5, c6) :: (c7, c8) :: (c9, c10) :: rem = ccs; *)

(* unify c1 c2 handle TypeCheckError s => print ("Type check failed, reason: " ^ s ^ "\n");
unify c3 c4 handle TypeCheckError s => print ("Type check failed, reason: " ^ s ^ "\n"); *)



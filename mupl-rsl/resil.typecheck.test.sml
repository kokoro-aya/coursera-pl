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

(*
    - getConstraints Resil.Env.empty f;
    val it =
      (VarT (1,ref NONE),
      [(VarT (1,ref NONE),VarT (9,ref NONE)),
        (VarT (11,ref NONE),FuncT (VarT (8,ref NONE),VarT (9,ref NONE))),
        (IntT,VarT (8,ref NONE)),
        (FuncT (VarT (2,ref NONE),FuncT (VarT (3,ref #),VarT (5,ref #))),
        FuncT (VarT (10,ref NONE),VarT (11,ref NONE))),
        (FuncT (VarT (12,ref NONE),VarT (13,ref NONE)),VarT (10,ref NONE)),
        (VarT (13,ref NONE),IntT),(IntT,VarT (12,ref NONE)),
        (VarT (12,ref NONE),IntT),
        (VarT (2,ref NONE),FuncT (VarT (4,ref NONE),VarT (5,ref NONE))),
        (VarT (7,ref NONE),VarT (4,ref NONE)),
        (VarT (2,ref NONE),FuncT (VarT (6,ref NONE),VarT (7,ref NONE))),
        (VarT (3,ref NONE),VarT (6,ref NONE))])
      : rslType * (rslType * rslType) list


*)

val f1 = Resil.Letrec (
  [
    ("double", Resil.Func("f", Resil.Func("#x", Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("f"), Resil.Var("#x")))))),
    ("x", Resil.Int(4))
  ],
  Resil.Var("double"));

val fx = Resil.Letrec (
  [
    ("x", Resil.Int(4))
  ],
  Resil.Var("x"));


(*
    - val (ty, ccs) = getConstraints Resil.Env.empty f1;

      val ty = VarT (1,ref NONE) : rslType
      val ccs =
        [(VarT (1,ref NONE),
          FuncT (VarT (2,ref NONE),FuncT (VarT (3,ref NONE),VarT (5,ref NONE)))),
        (VarT (2,ref NONE),FuncT (VarT (4,ref NONE),VarT (5,ref NONE))),
        (VarT (7,ref NONE),VarT (4,ref NONE)),
        (VarT (2,ref NONE),FuncT (VarT (6,ref NONE),VarT (7,ref NONE))),
        (VarT (3,ref NONE),VarT (6,ref NONE))] : (rslType * rslType) list

    - val (c1, c2) :: (c3, c4) :: (c5, c6) :: (c7, c8) :: (c9, c10) :: rem = ccs;

      stdIn:2.5-2.75 Warning: binding not exhaustive
                (c1,c2) :: (c3,c4) :: (c5,c6) :: (c7,c8) :: (c9,c10) :: rem = ...
      val c1 = VarT (1,ref NONE) : rslType
      val c2 = FuncT (VarT (2,ref NONE),FuncT (VarT (3,ref NONE),VarT (5,ref NONE)))
        : rslType
      val c3 = VarT (2,ref NONE) : rslType
      val c4 = FuncT (VarT (4,ref NONE),VarT (5,ref NONE)) : rslType
      val c5 = VarT (7,ref NONE) : rslType
      val c6 = VarT (4,ref NONE) : rslType
      val c7 = VarT (2,ref NONE) : rslType
      val c8 = FuncT (VarT (6,ref NONE),VarT (7,ref NONE)) : rslType
      val c9 = VarT (3,ref NONE) : rslType
      val c10 = VarT (6,ref NONE) : rslType
      val rem = [] : (rslType * rslType) list
- 
    - unify c10 c9;
      val it = () : unit
      - unify c8 c7;
      val it = () : unit
      - unify c6 c5;
      val it = () : unit
      - c4;
      val it = FuncT (VarT (4,ref (SOME (ParamT "B"))),VarT (5,ref NONE)) : rslType
      - c3;
      val it = 
        VarT (2,ref (SOME (FuncT (VarT (6,ref (SOME (ParamT "A")))),VarT (VarT (7,ref (SOME (ParamT "B")))))))
        : rslType


      - c10;
      val it = VarT (6,ref (SOME (ParamT "A"))) : rslType
      - c9;
      val it = VarT (3,ref (SOME (ParamT "A"))) : rslType
      - c8;
      val it =
        FuncT (VarT (6,ref (SOME (ParamT "A"))),VarT (7,ref (SOME (ParamT "B"))))
        : rslType
      - c7;
      val it =
        VarT (2,ref (SOME (FuncT (VarT (6,ref (SOME #)),VarT (7,ref (SOME #))))))
        : rslType
      - c6;
      val it = VarT (4,ref (SOME (ParamT "B"))) : rslType
      - c5;
      val it = VarT (7,ref (SOME (ParamT "B"))) : rslType
      - 
*)

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



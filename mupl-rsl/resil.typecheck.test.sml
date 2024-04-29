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
        ("x", Resil.Int(3)),
        ("y", Resil.Int(4))
      ],
      Resil.Binop(Resil.ADD, Resil.Var("y"), Resil.Var("x"))
    )

val letrec3 = Resil.Letrec (
    [
      ("f", Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1)))),
      ("g", Resil.Func("x", Resil.Binop(Resil.MULT, Resil.Var("x"), Resil.Int(2)))),
      ("z", Resil.Int(3))
    ],
    Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("g"), Resil.Var("z"))))

(* Calls *)

(* Call dyns *)

(* Complex cases *)

(* val (ty, ccs) = getConstraints Resil.Env.empty pair1; *)

(* val (c1, c2) :: (c3, c4) :: (c5, c6) :: rem = ccs; *)

(* unify c1 c2 handle TypeCheckError s => print ("Type check failed, reason: " ^ s ^ "\n");
unify c3 c4 handle TypeCheckError s => print ("Type check failed, reason: " ^ s ^ "\n"); *)

(*

- c3;
val it = VarT (3,ref (SOME (PairT (VarT (1,ref NONE),VarT (2,ref NONE)))))
  : rslType
- c4;
val it = PairT (VarT (4,ref NONE),UnitT) : rslType

*)

(*
- val cc1 = getConstraints Resil.Env.empty pair1;
val cc1 =
  (VarT (C,ref NONE),
   [(VarT (A,ref NONE),PairT (VarT (C,ref NONE),VarT (D,ref NONE))),
    (VarT (A,ref NONE),PairT (VarT (B,ref NONE),UnitT)),
    (VarT (B,ref NONE),PairT (StrT,IntT))])
  : rslType * (rslType * rslType) list


- val cc2 = getConstraints Resil.Env.empty ex;
val cc2 =
  (VarT (A,ref NONE),
   [(VarT (A,ref NONE),PairT (VarT (B,ref NONE),UnitT)),
    (VarT (B,ref NONE),PairT (StrT,IntT))])
  : rslType * (rslType * rslType) list

*)

(*
    val cc1 = getConstraints Resil.Env.empty pair1;
    val (ct, ccs1) = cc1;
    val (c1, c2) :: (c3, c4) :: (c5, c6) :: rem = ccs1;
*)

(*
- c1;
val it = VarT (7,ref (SOME (PairT (VarT (8,ref (SOME #)),UnitT)))) : rslType
- c2;
val it = PairT (VarT (5,ref NONE),VarT (6,ref NONE)) : rslType
- case c1 of VarT (_, rf) => (!rf);
stdIn:8.1-8.33 Warning: match nonexhaustive
          VarT (_,rf) => ...
  
val it = SOME (PairT (VarT (8,ref (SOME (PairT (StrT,IntT)))),UnitT))
  : rslType option



*)
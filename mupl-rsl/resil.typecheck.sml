use "resil.sig.sml";
use "resil.lang.sml";

exception TypeCheckError of string

datatype rslType = IntT
                 | BoolT
                 | StrT
                 | UnitT
                 | PairT of rslType * rslType
                 | FuncT of rslType * rslType
                 | ParamT of string
                 | VarT of int * rslType option ref

(* Unique identifier generator starts *)

val paramCharCount = ref 1

val varCount = ref 1

(* Generation methods *)

fun toCharCodeList num = 
  if num < 1 
    then [1]
    else if num <= 26
      then [num]

      else toCharCodeList (num div 26) @ [let val tl = num mod 26 in if tl = 0 then 26 else tl end]

fun toChars numList = List.map (fn c => Char.chr (c + 64)) numList

fun joinToString xs = case xs of
        [] => ""
      | x :: xs => Char.toString x ^ joinToString xs

(* Unique identifier generator ends *)

fun newParamType (): rslType = 
  let val nextCode = !paramCharCount in
    paramCharCount := (!paramCharCount) + 1;
    let val label = joinToString (toChars (toCharCodeList nextCode)) in
      ParamT label
    end
  end


fun newVarType (): rslType = 
  let val res = VarT ((!varCount), ref NONE) in
    varCount := (!varCount) + 1;
    res
  end


fun typeToString (typ: rslType): string = case typ of
        IntT => "int"
      | BoolT => "bool"
      | StrT => "string"
      | UnitT => "unit"
      | PairT (l, r) => "(" ^ typeToString l ^ ", " ^ typeToString r ^ ")"
      | FuncT (a, r) => typeToString a ^ " => " ^ typeToString r
      | ParamT s => "?" ^ s
      | VarT (i, ty) => "X" ^ Int.toString i ^ ":" ^ (case (!ty) of
                    NONE => "_"
                  | SOME ity => typeToString ity)


fun optTypToString (tyO: rslType option): string = case tyO of
        SOME x => typeToString x
      | NONE => "_"

fun getConstraints (env: rslType Resil.Env.env)  (exp: Resil.rslExp): rslType * (rslType * rslType) list =
      case exp of
          Resil.Int _ => (IntT, [])
        | Resil.Bool _ => (BoolT, [])
        | Resil.Str _ => (StrT, [])
        | Resil.Var s => (case Resil.Env.lookup env s of
              SOME x => (x, [])
            | NONE => raise TypeCheckError ("unknown variable " ^ s))
        | Resil.Binop (_, e1, e2) =>
          let val t = newVarType()
              val (t1, cons1) = getConstraints env e1
              val (t2, cons2) = getConstraints env e2
              val allCons = (t, t1) :: (t1, t2) :: (t2, IntT) :: (cons1 @ cons2)
          in (t, allCons)
          end
        | Resil.Logop (_, e1, e2) =>
          let val t = newVarType()
              val (t1, cons1) = getConstraints env e1
              val (t2, cons2) = getConstraints env e2
              val allCons = (t, BoolT) :: (t1, t2) :: (cons1 @ cons2)
          in (t, allCons)
          end
        | Resil.If (cond, caseT, caseF) =>
          let val t = newVarType()
              val (t1, cons1) = getConstraints env cond
              val (t2, cons2) = getConstraints env caseT
              val (t3, cons3) = getConstraints env caseF
              val allCons = (t, t2) :: (t, t3) :: (t2, t3) :: (t1, BoolT) :: (cons1 @ cons2 @ cons3)
          in (t, allCons)
          end
        | Resil.Func (_, exp) => 
          let val t1 = newVarType()
              val (t2, cons) = getConstraints env exp
          in (FuncT(t1, t2), cons)
          end
        | Resil.Call (funExp, actual) =>
          let val x1 = newVarType()
              val x2 = newVarType()
              val (t1, cons1) = getConstraints env funExp
              val (t2, cons2) = getConstraints env actual
              val allCons = (t1, FuncT(x1, x2)) :: (t2, x1) :: (cons1 @ cons2)
          in (x2, allCons)
          end
        | Resil.CallDyn (funName, actual) => raise TypeCheckError "dynamic call is currently unsupported"
        | Resil.Letrec (assigns, body) =>
          let val t = newVarType()
              val uncheckedEnvs: (string * rslType) list = List.foldl (fn ((s, v), acc) => 
                  let val (ty, cons) = getConstraints env v
                  in
                    (s, ty) :: acc
                  end
                ) [] assigns
              val newEnv = List.foldl (fn ((s, ty), acc) => 
                  Resil.Env.insert acc (s, ty)
                ) env uncheckedEnvs
              val (t1, cons1) = getConstraints newEnv body
              val allCons = (t, t1) :: cons1
          in (t, allCons)
          end
        | Resil.Pair (fst, snd) =>
          let val t = newVarType()
              val (t2, cons1) = getConstraints env fst
              val (t3, cons2) = getConstraints env snd
              val allCons = (t, PairT(t2, t3)) :: cons1 @ cons2
          in (t, allCons)
          end
        | Resil.IsAPair e => 
          let val t1 = newVarType()
              val t2 = newVarType()
              val (t3, cons) = getConstraints env e
              val allCons = (t3, PairT(t1, t2)) :: cons
          in (BoolT, allCons)
          end
        | Resil.Fst e =>
          let val t1 = newVarType()
              val t2 = newVarType()
              val (t3, cons) = getConstraints env e
              val allCons = (t3, PairT(t1, t2)) :: cons
          in (t1, allCons)
          end
        | Resil.Snd e => 
          let val t1 = newVarType()
              val t2 = newVarType()
              val (t3, cons) = getConstraints env e
              val allCons = (t3, PairT(t1, t2)) :: cons
          in (t2, allCons)
          end
        | Resil.Unit => (UnitT, [])


fun unify (left: rslType) (right: rslType) = case (left, right) of
    (IntT, IntT) => ()
  | (BoolT, BoolT) => ()
  | (StrT, StrT) => ()
  | (UnitT, UnitT) => ()
  | (PairT(p1, p2), PairT(q1, q2)) => 
    let val _ = unify p1 q1 in unify p2 q2 end
  | (FuncT(f1, r1), FuncT(f2, r2)) => 
    let val _ = unify f1 f2 in unify r1 r2 end
  | (VarT (i, rf), VarT (j, sf)) => 
    if i = j then
      if rf = sf then () else raise TypeCheckError ("Duplicated type variable #" ^ Int.toString i)
    else
      (case ((!rf), (!sf)) of
          (NONE, NONE) => raise TypeCheckError "Attempt to match different empty variables"
        | (SOME ty, NONE) =>
            sf := SOME ty
        | (NONE, SOME ty) =>
            rf := SOME ty
        | (SOME ty1, SOME ty2) =>
            if ty1 = ty2 then ()
            else raise TypeCheckError ("L171: type variable " ^ Int.toString i ^ ":" ^ optTypToString (!rf) ^ " is not the same as " ^ Int.toString j ^ ":" ^ optTypToString (!sf))
      )
  | (VarT (i, rf), _) => 
      (case (!rf) of
          NONE => 
            if containsType right left then raise TypeCheckError ("L175: Type " ^ typeToString right ^ " contains type " ^ typeToString left)
            else rf := SOME right
        | SOME ty => 
          if ty = right then () else unify ty right)
          (* raise TypeCheckError ("L176: left-hand type variable does not hold same type as right-hand side" ^ typeToString right ^ ", actual: " ^ typeToString ty) *) 
  | (_, VarT (i, rf)) => 
      (case (!rf) of
          NONE => 
            if containsType left right then raise TypeCheckError ("L183: Type " ^ typeToString left ^ " contains type " ^ typeToString right)
            else rf := SOME left
        | SOME ty =>
          if ty = left then () else unify ty left)
          (* if ty = left then () else raise TypeCheckError ("L185: right-hand type variable does not hold same type as left-hand side" ^ typeToString left ^ ", actual: " ^ typeToString ty)) *)
  | _ => raise TypeCheckError ("L188: Type check error with " ^typeToString left ^ " and " ^typeToString right ^ "\n")


and containsType (typ1: rslType) (typ2: rslType): bool = case typ1 of
    IntT => false
  | BoolT => false
  | StrT => false
  | UnitT => false
  | PairT(t1, t2) => containsType t1 typ2 orelse containsType t2 typ2
  | FuncT(t1, t2) => containsType t1 typ2 orelse containsType t2 typ2
  | VarT (i, rf) =>
      (case (!rf) of
          SOME v => (
            case typ2 of 
                VarT (j, _) => i = j 
              | _ => containsType v typ2)
        | NONE => (
            case typ2 of VarT (j, _) => i = j | _ => false))
  | ParamT par => 
      (case typ2 of
          ParamT p2 => par = p2
        | _ => false)

fun resolve (typ: rslType): rslType = case typ of
    IntT => IntT
  | BoolT => BoolT
  | StrT => StrT
  | UnitT => UnitT
  | PairT(t1, t2) => PairT(resolve t1, resolve t2)
  | FuncT(t1, t2) => FuncT(resolve t1, resolve t2)
  | VarT (_, ref (SOME t)) => resolve t
  | VarT (_, ref NONE) => newParamType ()
  | ParamT _ => typ


fun typecheck exp =
   let val (t, cons) = getConstraints Resil.Env.empty exp
   in (List.map (fn (l, r) => unify l r) (List.rev cons); 
      let val typ = resolve t
        in print ("Type inferred: [" ^ typeToString typ ^ "]\n")
      end)
   end handle TypeCheckError s => print ("Type check failed, reason: " ^ s ^ "\n")


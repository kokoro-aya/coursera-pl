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

fun resetParamType () =
  let val _ = paramCharCount := 1 in () end

fun newVarType (): rslType = 
  let val res = VarT ((!varCount), ref NONE) in
    varCount := (!varCount) + 1;
    res  end

fun resetVarType () = varCount := 1


fun typeToString (typ: rslType): string = case typ of
        IntT => "int"
      | BoolT => "bool"
      | StrT => "string"
      | UnitT => "unit"
      | PairT (l, r) => "(" ^ typeToString l ^ ", " ^ typeToString r ^ ")"
      | FuncT (a, r) => "(" ^ typeToString a ^ " => " ^ typeToString r ^ ")"
      | ParamT s => "?" ^ s
      | VarT (i, ty) => "X" ^ Int.toString i ^ ":" ^ (case (!ty) of
                    NONE => "_"
                  | SOME ity => 
                  typeToString ity)


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
        | Resil.Func (ar, exp) => 
          let val t1 = newVarType()
              val (t2, cons) = getConstraints (Resil.Env.insert env (ar, t1)) exp
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
              (*
                  Resolve typing for each variable of the letrec block from top to button, and augmenting the environment `accEnv`
                  after each iteration.
                  Once the type is resolved, a new type var is added and a new constraint links it to this type.
                  Whether this constraint is needed or not is unsure.
              *)
              val (consList, uncheckedEnvs) = List.foldl (fn ((s, v), (acc, accEnv)) => 
                  let val (ty, cons) = getConstraints accEnv v
                      val newVar = newVarType ()
                  in
                    (((s, ty), (ty, newVar) :: cons) :: acc, Resil.Env.insert accEnv (s, ty))
                  end
                ) ([], Resil.Env.empty) assigns

              (* Prepend the new environment to existing env list *)
              val newEnv = Resil.Env.concat uncheckedEnvs env
              (* Flatten the constraint List *)
              val newCons = List.foldl (fn ((_, cx), accCons) =>
                cx @ accCons
              ) [] consList

              (* Debug commands *)
              (* val _ = List.map (fn ((_, ty), pairs) => let 
                      
                      val _ = print "\n>>>\n"
                      val _ = print (typeToString ty ^ "\t") 
                      val _ = List.map (fn (f, s) => print ((typeToString f) ^ " ; " ^ (typeToString s) ^ "\t")) pairs
                      in () end ) consList *)

              val (t1, cons1) = getConstraints newEnv body
              val allCons = (t, t1) :: cons1 @ newCons
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
    (* Assert that two type vars with same index should be equal *)
    if i = j then
      if rf = sf then () else raise TypeCheckError ("Duplicated type variable #" ^ Int.toString i)
    else
      (* Unification of two different type vars *)
      (case ((!rf), (!sf)) of
          (* Case both are empty, since they should be equivalent, we assign a new type param => 
                it could be later set to concrete type or left abstract *)
          (NONE, NONE) => 
            let val newPara = newParamType ()
            in rf := SOME newPara ; sf := SOME newPara
            end
        | (SOME ty, NONE) =>
            sf := SOME ty
        | (NONE, SOME ty) =>
            rf := SOME ty
          (* Both sides holds something ... *)
        | (SOME ty1, SOME ty2) =>
            (case (ty1, ty2) of
                (* We now know that two type params are equivalent, so one of them can be substituted by another *)
                (ParamT s1, ParamT s2) => if s1 = s2 then ()
                  else
                    rf := SOME ty2
                (* Cases where a type param is equivalent to a concrete type, so it could be substituted by it *)
              | (ParamT _, IntT) => rf := SOME ty2
              | (ParamT _, BoolT) => rf := SOME ty2
              | (ParamT _, StrT) => rf := SOME ty2
              | (ParamT _, UnitT) => rf := SOME ty2
              | (ParamT _, PairT _) => rf := SOME ty2
              | (ParamT _, FuncT _) => rf := SOME ty2
                (* Mirror cases *)
              | (IntT, ParamT _) => sf := SOME ty1
              | (BoolT, ParamT _) => sf := SOME ty1
              | (StrT, ParamT _) => sf := SOME ty1
              | (UnitT, ParamT _) => sf := SOME ty1
              | (PairT _, ParamT _) => sf := SOME ty1
              | (FuncT _, ParamT _) => sf := SOME ty1
                (* Not concerned, proceed to unification of what it holds *)
              | _ => unify ty1 ty2
            ))
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
    (* Resolution of params in top level
          Ideally, this arm should be handled with upper side where params are handled inside refs
     *)
  | (ParamT s1, ParamT s2) => 
      if s1 = s2 then () else raise TypeCheckError ("L211: Param clash with " ^ s1 ^ " and " ^ s2)
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
  | VarT (_, rf) => 
    (case (!rf) of
        SOME t => resolve t
      | NONE => let val newPara = newParamType ()
                    val _ = rf := SOME newPara
                in newPara end)
  | ParamT _ => typ


fun typecheck exp =
   let val (t, cons) = getConstraints Resil.Env.empty exp
   in (List.map (fn (l, r) => unify l r) (List.rev cons); 
      let val typ = resolve t
          val _ = resetParamType ()
          val _ = resetVarType ()
        in print ("Type inferred: [" ^ typeToString typ ^ "]\n")
      end)
   end handle TypeCheckError s => print ("Type check failed, reason: " ^ s ^ "\n")


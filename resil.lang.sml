use "resil.sig.sml";

structure Resil :> RSL =
struct

  exception EvalError of string

  (* This part is tested in detached file envtest.sml since it's hard to test here *)
  structure Env :> ENV = struct
    type 'a env = (string *'a) list
    val empty = nil

    fun insert env kv = kv :: env

    fun lookup env k = case env of
        [] => NONE
      | (a, b) :: xs => if a = k then SOME b else lookup xs k

    (* Only update the latest binding *)
    fun update env s newV = case env of
        [] => []
      | (a, b) :: xs => if a = s then (a, newV) :: xs else (a, b) :: update xs s newV

    fun concat env1 env2 = env1 @ env2
  end

  datatype binop = ADD | SUB | MULT | DIV | MOD
  datatype logop = EQ | LT | LE | NEQ

  datatype rslVal =
      IntV of int
    | BoolV of bool
    | PairV of rslVal * rslVal
    | UnitV
    | ClosV of { env: rslVal Env.env, f: rslExp }
    | PromV of rslVal option ref  (* Promise value, for use with letrec *)
  
  and rslExp =
      Int of int
    | Bool of bool
    | Var of string
    | Binop of binop * rslExp * rslExp
    | Logop of logop * rslExp * rslExp
    | If of rslExp * rslExp * rslExp
    | Func of string * rslExp
    | Call of rslExp * rslExp
    | Letrec of (string * rslExp) list * rslExp
    | Pair of rslExp * rslExp
    | Fst of rslExp
    | Snd of rslExp
    | Unit

    fun show v =
      case v of
          IntV (i) => Int.toString(i) 
        | BoolV (b) => Bool.toString(b)
        | PairV (l, r) => "(" ^ show l ^ "," ^ show r ^ ")"
        | UnitV => "unit"
        | ClosV _ => "#closure"
        | PromV _ => "#promise"
        

    fun typ v =
      case v of
          IntV _ => "int"
        | BoolV _ => "bool"
        | PairV (a, b) => "(" ^ typ a ^ ", " ^ typ b ^ ")"
        | ClosV _ => "closure"
        | UnitV => "unit"
        | PromV _ => "promise"
        
    fun evalEnv env exp =
        case exp of
            Int (i) => IntV i
          | Bool (b) => BoolV b
          | Var (s) =>
              (case Env.lookup env s of
                  SOME x => 
                    (case x of
                        PromV (r) => 
                          (case (!r) of
                              SOME x => x
                            | NONE => raise EvalError ("Reading an unset promise while resolving variable name " ^ s)
                          )
                      | _ => x
                    )
                | NONE => raise EvalError ("Unresolved variable name " ^ s)
              )
          | Binop (oper, l, r) =>
              (case (evalEnv env l, evalEnv env r) of
                    (IntV i1, IntV i2) => 
                        (case oper of
                            ADD => IntV (i1 + i2)
                          | SUB => IntV (i1 - i2)
                          | MULT => IntV (i1 * i2)
                          | DIV => IntV (i1 div i2)
                          | MOD => IntV (i1 mod i2)
                        )
                  | (v1, v2) => raise EvalError ("Binop expects integers. Instead found: "
                            ^ typ (PairV (v1, v2))))
          | Logop (oper, l, r) =>
                (case (evalEnv env l, evalEnv env r) of
                    (BoolV b1, BoolV b2) => 
                        (case oper of
                            EQ => BoolV (b1 = b2)
                          | NEQ => BoolV (b1 <> b2)
                          | _ => raise EvalError ("Logop EQ or NEQ expected for bools.")
                        )
                  | (IntV i1, IntV i2) =>
                        (case oper of
                            LT => BoolV (i1 < i2)
                          | LE => BoolV (i1 <= i2)
                          | _ => raise EvalError ("Logop LT or LE expected for ints.")
                        )
                  | (v1, v2) => raise EvalError ("Logop expects either ints or bools. Instead found: "
                            ^ typ (PairV (v1, v2))))
          | If (c, t, e) => 
                (case evalEnv env c of
                    BoolV (b) => if b then evalEnv env t else evalEnv env e
                  | _ => raise EvalError "If operation applied to non-bool"
                )
          | Func (s, exp) => ClosV { env = env, f = exp }
          | Call (funexp, actual) =>
              let 
                val vFn = evalEnv env funexp
                val vAct = evalEnv env actual in
                (case vFn of
                    ClosV { env, f } =>
                      (case funexp of
                          Func (funArg, funBody) =>
                            let val newEnv = Env.empty
                                val currEnv = Env.insert newEnv (funArg, vAct)
                            in
                              evalEnv (Env.concat env currEnv) funBody
                            end
                        | _ => raise EvalError "MUPL call operation must have funexp subexpression of closure evaluated to Func")
                  | _ => raise EvalError "MUPL call operation must have first subexpression evaluated to closure")
              end
          | Letrec (exps, body) =>
              let
                val prefilledEnvs = List.foldl (fn ((s, _), acc) => Env.insert acc (s, PromV (ref NONE))) Env.empty exps
                val evaledEnvs = List.foldl (fn ((si, ei), acc) =>
                  let val exi = evalEnv acc ei in
                    Env.update acc si exi 
                  end
                ) prefilledEnvs exps (* (string * rslVal) list aka rslVal env *)
              in
                evalEnv (Env.concat evaledEnvs env) body
              end
          | Pair (e1, e2) =>
              let
                val v1 = evalEnv env e1
                val v2 = evalEnv env e2 in
                  PairV (v1, v2)
              end
          | Fst (e) =>
              let val v = evalEnv env e in
                (case v of
                    PairV (e1, _) => e1 
                  | _ => raise EvalError "fst operation applied to non-pair"
                )
              end
          | Snd (e) =>
              let val v = evalEnv env e in
                (case v of
                    PairV (_, e2) => e2 
                  | _ => raise EvalError "snd operation applied to non-pair"
                )
              end
          | Unit => UnitV

    fun eval exp = evalEnv Env.empty exp
end
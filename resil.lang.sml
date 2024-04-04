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
    | StrV of string
    | PairV of rslVal * rslVal
    | UnitV
    | ClosV of { env: rslVal Env.env, f: rslExp }
    | PromV of rslVal option ref  (* Promise value, for use with letrec *)
    | ErrV of string
  
  and rslExp =
      Int of int
    | Bool of bool
    | Str of string
    | Var of string
    | Binop of binop * rslExp * rslExp
    | Logop of logop * rslExp * rslExp
    | If of rslExp * rslExp * rslExp
    | Func of string * rslExp
    | Call of rslExp * rslExp
    | CallDyn of rslExp * rslExp
    | Letrec of (string * rslExp) list * rslExp
    | Pair of rslExp * rslExp
    | IsAPair of rslExp
    | Fst of rslExp
    | Snd of rslExp
    | Unit

    fun showExp exp =
      case exp of
          Int (i) => "Int(" ^ Int.toString(i) ^ ")"
        | Bool (b) => "Bool(" ^ Bool.toString(b) ^ ")"
        | Str (s) => "Str(" ^ s ^ ")"
        | Var (s) => "Var(" ^ s ^")"
        | Binop _ => "BinOp"
        | Logop _ => "LogOp"
        | If _ => "If" 
        | Func (s, e) => "Func(" ^ s ^ ", " ^ showExp e ^ ")" 
        | Call _ => "Call"
        | CallDyn _ => "CallDyn"
        | Letrec _ => "Letrec"
        | Pair _ => "Pair"
        | IsAPair _ => "IsAPair"
        | Fst _ => "Fst"
        | Snd _ => "Snd"
        | Unit => "Unit"

    fun show v =
      case v of
          IntV (i) => Int.toString(i) 
        | BoolV (b) => Bool.toString(b)
        | StrV (s) => s
        | PairV (l, r) => "(" ^ show l ^ "," ^ show r ^ ")"
        | UnitV => "unit"
        | ClosV _ => "#closure"
        | PromV _ => "#promise"
        | ErrV e => e
        

    fun typ v =
      case v of
          IntV _ => "int"
        | BoolV _ => "bool"
        | StrV _ => "str"
        | PairV (a, b) => "(" ^ typ a ^ ", " ^ typ b ^ ")"
        | ClosV _ => "closure"
        | UnitV => "unit"
        | PromV _ => "promise"
        | ErrV _ => "error"
        
    fun evalEnv env exp =
        case exp of
            Int (i) => IntV i
          | Bool (b) => BoolV b
          | Str (s) => StrV s
          | Var (s) =>
              (case Env.lookup env s of
                  SOME x => 
                    (case x of
                        PromV (r) => 
                          (case (!r) of
                              SOME x => x
                            | NONE => raise EvalError ("[1] Reading an unset promise while resolving variable name " ^ s)
                          )
                      | _ => x
                    )
                | NONE => raise EvalError ("[2] Unresolved variable name " ^ s)
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
                  | (v1, v2) => raise EvalError ("[3] Binop expects integers. Instead found: "
                            ^ typ (PairV (v1, v2))))
          | Logop (oper, l, r) =>
                (case (evalEnv env l, evalEnv env r) of
                    (BoolV b1, BoolV b2) => 
                        (case oper of
                            EQ => BoolV (b1 = b2)
                          | NEQ => BoolV (b1 <> b2)
                          | _ => raise EvalError ("[4] Logop EQ or NEQ expected for bools.")
                        )
                  | (IntV i1, IntV i2) =>
                        (case oper of
                            LT => BoolV (i1 < i2)
                          | LE => BoolV (i1 <= i2)
                          | _ => raise EvalError ("[5] Logop LT or LE expected for ints.")
                        )
                  | (v1, v2) => raise EvalError ("[6] Logop expects either ints or bools. Instead found: "
                            ^ typ (PairV (v1, v2))))
          | If (c, t, e) => 
                (case evalEnv env c of
                    BoolV (b) => if b then evalEnv env t else evalEnv env e
                  | _ => raise EvalError "[7] If operation applied to non-bool"
                )
          | Func _ => ClosV { env = env, f = exp }
          | Call (funexp, actual) =>
              let 
                val vFn = evalEnv env funexp
                val vAct = evalEnv env actual in
                (case vFn of
                    ClosV { env, f } => 
                      (case f of
                          Func (funArg, funBody) =>
                            let val newEnv = Env.empty
                                val currEnv = Env.insert newEnv (funArg, vAct)
                            in
                              evalEnv (Env.concat env currEnv) funBody
                            end

                        | _ => raise EvalError ("[8] call oper. must have a Func as f, got " ^ showExp f))
                
                  | _ => raise EvalError ("[9] call oper. must have first subexpr as Closure, got " ^ show vFn))
              end
          | CallDyn (dynName, actual) => 
              let
                val vFLabel = evalEnv env dynName
                val vAct = evalEnv env actual in
                (case vFLabel of
                    StrV (name) =>
                      (case Env.lookup env name of
                          SOME (ClosV { env, f }) =>
                            (case f of
                              Func (funArg, funBody) =>
                                let val newEnv = Env.empty
                                    val currEnv = Env.insert newEnv (funArg, vAct)
                                in
                                  evalEnv (Env.concat env currEnv) funBody
                                end
                              | _ => raise EvalError ("[10] call oper. must have a Func as f, got " ^ showExp f))
                          | NONE => raise EvalError ("[11] dyn call on unresolved name " ^ name))
                  | _ => raise EvalError ("[12] dyn call must be invoked on StrV(..), got " ^ show vFLabel))
              end
          | Letrec (exps, body) =>
              let
                val prefilledEnvs = List.foldl (fn ((s, _), acc) => Env.insert acc (s, PromV (ref NONE))) env exps
                val evaledEnvs = List.foldl (fn ((si, ei), acc) =>
                  let val exi = evalEnv acc ei in
                    Env.update acc si exi 
                  end
                ) prefilledEnvs exps (* (string * rslVal) list aka rslVal env *)
              in
                evalEnv evaledEnvs body
              end
          | Pair (e1, e2) =>
              let
                val v1 = evalEnv env e1
                val v2 = evalEnv env e2 in
                  PairV (v1, v2)
              end
          | IsAPair (e) =>
              let
                val v = evalEnv env e in
                (case v of
                    PairV _ => BoolV(true)
                  | _ => BoolV(false))
              end
          | Fst (e) =>
              let val v = evalEnv env e in
                (case v of
                    PairV (e1, _) => e1 
                  | _ => raise EvalError "[13] fst operation applied to non-pair"
                )
              end
          | Snd (e) =>
              let val v = evalEnv env e in
                (case v of
                    PairV (_, e2) => e2 
                  | _ => raise EvalError "[14] snd operation applied to non-pair"
                )
              end
          | Unit => UnitV

    fun eval exp = (evalEnv Env.empty exp) handle EvalError (msg) => ErrV (msg) 
end
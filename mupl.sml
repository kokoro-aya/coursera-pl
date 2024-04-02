use "signatures.sml";

structure Mupl :> MUPL =
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

      fun concat env1 env2 = env1 @ env2

    end
    datatype muplExp =
        Int of int
      | Var of string
      | Add of muplExp * muplExp
      | Func of string option * string * muplExp
      | IfGreater of muplExp * muplExp * muplExp * muplExp
      | Call of muplExp * muplExp
      | MLet of string * muplExp * muplExp
      | APair of muplExp * muplExp
      | Fst of muplExp
      | Snd of muplExp
      | IsAUnit of muplExp
      | AUnit

    datatype muplValue =
        IntV of int
      | APairV of muplValue * muplValue
      | Closure of { env: muplValue Env.env, f: muplExp }
      | AUnitV
      

    fun show v =
      case v of
          IntV (i) => Int.toString(i) 
        | APairV (l, r) => "(" ^ show l ^ "," ^ show r ^ ")"
        | Closure _ => "#closure"
        | AUnitV => "()"
        

    fun typ v =
      case v of
          IntV (i) => "int"
        | APairV (l, r) => "pair"
        | Closure _ => "#closure"
        | AUnitV => "unit"
        
    fun evalEnv env exp =
        case exp of
            Int i        => IntV i
          | Var s        => 
              (case Env.lookup env s of
                  SOME x => x
                | NONE => raise EvalError ("Unresolved variable name " ^ s)
              )
          | Add (e1, e2) =>
                (case (evalEnv env e1, evalEnv env e2) of
                    (IntV i1, IntV i2) => IntV (i1 + i2)
                  | (v1, v2) => raise EvalError ("Add expects integers. Instead found: "
                            ^ typ (APairV (v1, v2))))
          | Func _ => Closure { env = env, f = exp }
          | IfGreater (e1, e2, e3, e4) =>
              let 
                val v1 = evalEnv env e1
                val v2 = evalEnv env e2 in 
                case (v1, v2) of
                    (IntV(i1), IntV(i2)) =>
                    if i1 > i2 then evalEnv env e3 else evalEnv env e4
                  | _ => raise EvalError "MUPL ifgreater operation applied to non-number"
              end
          | Call (funexp, actual) =>
              let 
                val vFn = evalEnv env funexp
                val vAct = evalEnv env actual in
                (case vFn of
                    Closure { env, f } =>
                      (case f of
                          Func (funName, funArg, funBody) =>
                            let val newEnv = Env.empty
                                val currEnv = 
                                    case funName of
                                        SOME fx => Env.insert (Env.insert newEnv (fx, vFn)) (funArg, vAct)
                              
                                      | NONE => Env.insert newEnv (funArg, vAct)
                            in
                              evalEnv (Env.concat env currEnv) funBody
                            end
                        | _ => raise EvalError "MUPL call operation must have inner subexpression of closure evaluated to Func")
                  | _ => raise EvalError "MUPL call operation must have first subexpression evaluated to closure")
              end
          | MLet (v, e, body) =>
              let val ex = evalEnv env e in
                evalEnv (Env.insert env (v, ex)) body
              end
          | APair (e1, e2) =>
              let
               val v1 = evalEnv env e1
               val v2 = evalEnv env e2 in
                APairV (v1, v2)
              end
          | Fst (e) => 
              let val v = evalEnv env e in
                (case v of
                    APairV (e1, _) => e1 
                  | _ => raise EvalError "MUPL fst operation applied to non-pair"
                )
              end
          | Snd (e) =>
              let val v = evalEnv env e in
                (case v of
                    APairV (_, e2) => e2 
                  | _ => raise EvalError "MUPL fst operation applied to non-pair"
                )
              end
          | IsAUnit (e) => (case e of
                APair _ => IntV 1
              | _ => IntV 0
            )
          | AUnit => AUnitV

    fun eval exp = evalEnv Env.empty exp


    fun expectIntPair (lrpair: muplValue * muplValue) (func: (int * int) -> 'a) (errmsg: string): 'a =
      case lrpair of
          (IntV(x), IntV(y)) => func(x, y)
        | _ => raise EvalError errmsg

    fun expectPair (e: muplValue) (func: (muplValue * muplValue) -> 'a) (errmsg: string): 'a =
      case e of
          APairV (a, b) => func(a, b)
        | _ => raise EvalError errmsg
end
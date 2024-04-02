use "env.sig.sml";

signature MUPL =
sig
    exception EvalError of string
    structure Env : ENV

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

   val show: muplValue -> string
   val typ: muplValue -> string
   val evalEnv: muplValue Env.env -> muplExp -> muplValue
   val eval: muplExp -> muplValue
end
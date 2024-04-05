use "resil.sig.sml";
use "resil.lang.sml";

datatype rslType = IntT
                 | BoolT
                 | UnitT
                 | PairT of rslType * rslType
                 | FuncT of rslType * rslType
                 | ParamT of string
                 | VarT of int * rslType option ref

val paramCharCount = ref 1

val varCount = ref 1

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
      | UnitT => "unit"
      | PairT (l, r) => "(" ^ typeToString l ^ ", " ^ typeToString r ^ ")"
      | FuncT (a, r) => typeToString a ^ " => " ^ typeToString r
      | ParamT s => "?" ^ s
      | VarT (i, _) => "X" ^ Int.toString i

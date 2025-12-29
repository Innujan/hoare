structure Datatypes = struct
  datatype Nexp = num of int | var of string | plus of Nexp * Nexp
  datatype Bexp = boolean of bool | imply of Bexp * Bexp | less of Nexp * Nexp | equal of Nexp * Nexp
  datatype Program = skip | concat of Program * Program | assign
    of string * Nexp | ifThenElse of Bexp * Program * Program | whileDo of Bexp *
    Program
  type triple = Bexp * Program * Bexp
  datatype token = tvar of string | timply | tequal | tless | tsemicol | tassign | tlbr | trbr

  fun toStringNexp(n: Nexp) =
    let
      val s = case n of num n => Int.toString(n)
                 | var n => n
                 | plus n => toStringNexp(#1 n) ^ " + " ^ toStringNexp(#2 n)
    in
      s
    end

  fun toStringBexp(b: Bexp) =
    let
      val s = case b of boolean b => Bool.toString(b)
                 | imply b => toStringBexp(#1 b) ^ " => " ^ toStringBexp(#2 b)
                 | less b => toStringNexp(#1 b) ^ " < " ^ toStringNexp(#2 b)
                 | equal b => toStringNexp(#1 b) ^ " = " ^ toStringNexp(#2 b)
    in
      s
    end

  fun toStringProgram(p: Program) =
    let
      val s = case p of skip => "skip"
                 | concat p => toStringProgram(#1 p) ^ "; " ^ toStringProgram(#2 p)
                 | assign p => (#1 p) ^ " := " ^ toStringNexp(#2 p)
                 | ifThenElse p => "if " ^ toStringBexp(#1 p) ^ " then " ^ toStringProgram(#2 p) ^ " else " ^ toStringProgram(#3 p)
                 | whileDo p => "while " ^ toStringBexp(#1 p) ^ " do " ^ toStringProgram(#2 p)
    in
      s
    end

  fun toStringTriple(t: triple) = "{" ^ toStringBexp(#1 t) ^ "} " ^ toStringProgram(#2 t) ^ "{" ^  toStringBexp(#3 t) ^ "}";

  (*fun boolLex [] = []
    | boolLex (#" " :: cs) = boolLex cs
    | boolLex (#"(" :: cs) = tlbr :: boolLex cs
    | boolLex (#")" :: cs) = trbr :: boolLex cs
    | boolLex (#"=>" :: cs) = timply :: boolLex cs
    | boolLex (#"=" :: cs) = tequal :: boolLex cs
    | boolLex (#"<" :: cs) = tless :: boolLex cs
    | boolLex (c :: cs) =
        if Char.isAlpha c then
          let fun getVar (x :: xs) = if Char.isAlphaNum x then getVar xs else (x::xs)
            | getVar [] = []
            val (v, rest) = (List.take(c::cs, length(c::cs) - length(getVar(c::cs))), getVar(c::cs))
          in
            tvar (String.implode v) :: boolLex rest end
        else boolLex cs*)

end

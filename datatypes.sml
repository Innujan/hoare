structure Datatypes = struct
  datatype Nexp = num of int | var of string | plus of Nexp * Nexp
  datatype Bexp = bul of bool | imply of Bexp * Bexp | less of Nexp * Nexp | equal of Nexp * Nexp
  datatype Program = skip | concatenate of Program * Program | assign
    of string * Nexp | ifThenElse of Bexp * Program * Program | whileDo of Bexp *
    Program
  type triple = Bexp * Program * Bexp

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
      val s = case b of bul b => Bool.toString(b)
                 | imply b => toStringBexp(#1 b) ^ " => " ^ toStringBexp(#2 b)
                 | less b => toStringNexp(#1 b) ^ " < " ^ toStringNexp(#2 b)
                 | equal b => toStringNexp(#1 b) ^ " = " ^ toStringNexp(#2 b)
    in
      s
    end

  fun toStringProgram(p: Program) =
    let
      val s = case p of skip => "skip"
                 | concatenate p => toStringProgram(#1 p) ^ "; " ^ toStringProgram(#2 p)
                 | assign p => (#1 p) ^ " := " ^ toStringNexp(#2 p)
                 | ifThenElse p => "if " ^ toStringBexp(#1 p) ^ " then " ^ toStringProgram(#2 p) ^ " else " ^ toStringProgram(#3 p)
                 | whileDo p => "while " ^ toStringBexp(#1 p) ^ " do " ^ toStringProgram(#2 p)
    in
      s
    end

  fun toStringTriple(t: triple) = "{" ^ toStringBexp(#1 t) ^ "} " ^ toStringProgram(#2 t) ^ "{" ^  toStringBexp(#3 t) ^ "}";
end

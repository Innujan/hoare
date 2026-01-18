structure PrettyPrint = struct
  open Datatypes

  fun toStringNexp(n: Nexp) =
	case n of 
	  num n => Int.toString(n)
	| var n => n
  | plus (n1, num n2) => if n2 < 0 then toStringNexp(n1) ^ " - " ^ Int.toString(~n2) else toStringNexp(n1) ^ " + " ^ Int.toString(n2)
	| plus (n1,n2) => toStringNexp(n1) ^ " + " ^ toStringNexp(n2)

  fun toStringBexp(b: Bexp) =
	case b of 
	  boolean b => Bool.toString(b)
    | imply (imply (p,imply (q,boolean false)),boolean false) => "(" ^ toStringBexp(p) ^ " & " ^ toStringBexp(q) ^ ")"   (* P ∧ Q *)
    | imply (less(n1, n2), boolean false) => toStringNexp(n1) ^ " ≥ " ^ toStringNexp(n2)
    | imply (b1,boolean false) => "~" ^ toStringBexp(b1)  (* ¬P *)
    | imply (imply(p, boolean false),b2) => "(" ^ toStringBexp(p) ^ " | " ^ toStringBexp(b2) ^ ")" (* P ∨ Q *)
    | imply (b1,b2) => "(" ^ toStringBexp(b1) ^ " => " ^ toStringBexp(b2) ^ ")"
	| less (n1,n2) => toStringNexp(n1) ^ " < " ^ toStringNexp(n2)
	| equal (n1,n2) => toStringNexp(n1) ^ " = " ^ toStringNexp(n2)

  fun toStringDNF(dnf: DNF) : string =
    let
      fun regionToString (r: Region) =
        "[" ^ String.concatWith ", " (List.map toStringBexp r) ^ "]"
    in
      "[" ^ String.concatWith ", " (List.map regionToString dnf) ^ "]"
    end

  fun toStringProgram(p: Program) =
    case p of 
	  skip => "skip"
	| concat (p1,p2) => toStringProgram(p1) ^ "; " ^ toStringProgram(p2)
    | assign (x,n) => toStringNexp(x) ^ " := " ^ toStringNexp(n)
    | ifThenElse (p1,p2,p3) => "if " ^ toStringBexp(p1) ^ " then " ^ toStringProgram(p2) ^ " else " ^ toStringProgram(p3)
    | whileDo (p1,p2) => "while " ^ toStringBexp(p1) ^ " do " ^ toStringProgram(p2)

  fun toStringTriple(t: triple) = "{" ^ toStringBexp(#1 t) ^ "} " ^ toStringProgram(#2 t) ^ " {" ^  toStringBexp(#3 t) ^ "}";
  fun toStringImplication(i: implication) = toStringBexp(#1 i) ^ " => " ^ toStringBexp(#2 i);
  fun toStringPremise (premise: Premise) = case premise of pimplication(p, q) => toStringImplication(p, q) | ptriple(t) => (case t of selectedtriple t => ("\u001b[1;31m" ^ toStringTriple(t) ^ "\u001b[0m")| normaltriple t => toStringTriple(t)) | pdummy (t) => t
  
  
  
	val lineChar = 
		if (OS.Process.system "ver" = OS.Process.success) then (* ver command only exists in windows *)
			String.implode [Char.chr 196]  (* CP437 *)
		else
			"─"          (* Unicode *)
			
  fun toStringRule (Rule {premises, conclusion} ) =
    let
		
		val premiseStrings = List.map toStringPremise premises
		val premisesLength = List.foldl (fn (s, acc) => String.size s - (if String.size s > 0 andalso String.sub(s,0) = #"\u001b" then 10 else 0) + acc) 0 premiseStrings
        val c = (case conclusion of selectedtriple t => ("\u001b[1;31m" ^ toStringTriple(t) ^ "\u001b[0m")| normaltriple t => toStringTriple(t))
		
        val rowwidth = Int.max(64, Int.max(premisesLength + 10, String.size c + 10))
		val premisePadding = (rowwidth - premisesLength) div (List.length(premises) + 1)

        val premisestring = String.concat (List.map (fn pistring => String.implode(List.tabulate(premisePadding, fn _ => #" ")) ^ pistring) premiseStrings) ^ "\n"

        val u = String.concat(List.tabulate(rowwidth, fn _ => lineChar)) ^ "\n"

        val conclusionstring = String.implode(List.tabulate((rowwidth - (String.size c - (if String.size c > 0 andalso String.sub(c,0) = #"\u001b" then 10 else 0))) div 2, fn _ => #" ")) ^ c ^ "\n"
    in
        premisestring ^ u ^ conclusionstring
    end

end
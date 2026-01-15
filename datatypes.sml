structure Datatypes = struct
  datatype Nexp = num of int | var of string | plus of Nexp * Nexp
  datatype Bexp = boolean of bool | imply of Bexp * Bexp | less of Nexp * Nexp | equal of Nexp * Nexp
  datatype Program = skip | concat of Program * Program | assign of Nexp * Nexp | ifThenElse of Bexp * Program * Program | whileDo of Bexp * Program
  type triple = Bexp * Program * Bexp
  type implication = Bexp * Bexp
  
  datatype DisplayTriple = selectedtriple of triple | normaltriple of triple
  datatype Premise = pimplication of Bexp * Bexp | ptriple of DisplayTriple | pdummy of string
  datatype Rule =
    Rule of {
        premises : Premise list,
        conclusion : DisplayTriple
    }
	
  datatype StepData =
    StepData of {
		triple : triple,
		rule : Rule,
		Q : Bexp option
    }
  
  datatype StepResult =
	StepResult of {
		stepdata : StepData list,
		message : string
	}

  fun boolnot(b: Bexp) = imply(b, boolean(false))
  fun boolor(b1: Bexp, b2: Bexp) = imply(boolnot(b1), b2)
  fun booland(b1: Bexp,b2: Bexp) = boolnot(imply(b1, boolnot(b2)))
  
  
	fun sortString [] = []
	  | sortString (x::xs) =
      let
        val smaller = List.filter (fn y => 
                        case String.compare(y, x) of
                            LESS => true
                          | _ => false
                      ) xs
        val larger  = List.filter (fn y => 
                        case String.compare(y, x) of
                            LESS => false
                          | _ => true
                      ) xs
      in
        sortString smaller @ [x] @ sortString larger
      end
  
  fun standardizeNexp(n: Nexp) =
      (*standardizes Nexp by making sums consistently have numbers in root's right node, and left node contains vars recursively in alphabetical order*)
	  let
		fun collect n =
		  case n of
			  num k => ([], k)
			| var x => ([x], 0)
			| plus(n1, n2) =>
				let
				  val (vars1, c1) = collect n1
				  val (vars2, c2) = collect n2
				in
				  (vars1 @ vars2, c1 + c2)
				end
		fun rebuild (vars, c) =
		  let
			val sortedVars = sortString vars
			val varNexp =
			  case sortedVars of
				  [] => NONE
				| [v] => SOME (var v)
				| v::vs =>
					let
					  (* recursively build nested pluses *)
					  fun foldVars [] = NONE
						| foldVars [x] = SOME (var x)
						| foldVars (x::xs) =
							case foldVars xs of
								NONE => SOME (var x)
							  | SOME rest => SOME (plus(var x, rest))
					in
					  foldVars (v::vs)
					end
		  in
			case (varNexp, c) of
			    (NONE, k) => num k
			  | (SOME v, 0) => v
			  | (SOME v, k) => plus(v, num k)
		  end
	  in
		rebuild (collect n)
	  end

  fun standardizeBexp(b: Bexp) =
    case b of
	  boolean _ => b
	| imply (b1,b2) =>
	  let
	    val i1 = standardizeBexp(b1)
		val i2 = standardizeBexp(b2)
		fun getVarsLHPlus(v) = case v of
			(plus (s, num n)) => SOME s
			| num n => NONE
			| _ => SOME v
		fun getNumPlus(v) = case v of
			(plus (s, num n)) => n
			| num n => n
			| _ => 0
	  in
	    case (i1,i2) of
          (boolean true,b) => b
        | (boolean false,_) => boolean true
		| (_,boolean true) => boolean true
		| (less (v1, v2), boolean false) => standardizeBexp(less(v2,plus(v1,num 1)))
		| (less (v1, v2), less (w1, w2)) => (
			let
				val varsidev = getVarsLHPlus(v1)
				val numsv = getNumPlus(v1)
				val varsidew = getVarsLHPlus(w1)
				val numsw = getNumPlus(w1)
			in
				if(varsidev = varsidew andalso v2 = w2 andalso numsw <= numsv) then
					boolean true
				else
					boolean false
			end
		)
		| (less _, equal _) => boolean false
		| (equal (v1, v2), boolean false) => standardizeBexp(boolor(less(v1,v2), less(v2,v1)))
		| (equal (v1, v2), less (w1, w2)) => standardizeBexp(boolor(imply(less(v1,v2),less(w1, w2)),imply(less(v2,v1),less(w1, w2))))
		| (equal (v1, v2), equal (w1, w2)) => if v1=w1 andalso v2=w2 then boolean true else boolean false
		| (_,_) => if i1 = i2 then boolean true else imply (i1,i2) (*this line never gets called since the recursion is complete; it's just here to avoid warnings*)
	  end
	| less (n1,n2) =>
	  let
	    val v1 = standardizeNexp(n1)
		val v2 = standardizeNexp(n2)
	  in
	    case (v1,v2) of
		  (num m,num n) => boolean (m<n)
		| (_,num n) => less (standardizeNexp(plus(v1,num (~n))),standardizeNexp(plus(v2,num (~n)))) (*moves numbers to lhs*)
		| (_,plus(_, num n)) => less (standardizeNexp(plus(v1,num (~n))),standardizeNexp(plus(v2,num (~n)))) (*moves numbers to lhs*)
		| (_,_) => less (v1,v2)
	  end
	| equal (n1,n2) =>
	  let
		val ns = 
		  let
			val v1 = standardizeNexp(n1)
			val v2 = standardizeNexp(n2)
		  in
			case (v1,v2) of
			  (num m,num n) => boolean (m=n)
			| (_,num n) => equal (standardizeNexp(plus(v1,num (~n))),standardizeNexp(plus(v2,num (~n)))) (*moves numbers to lhs*)
			| (_,plus(_, num n)) => equal (standardizeNexp(plus(v1,num (~n))),standardizeNexp(plus(v2,num (~n)))) (*moves numbers to lhs*)
			| (_,_) => equal (v1,v2)
		  end
	  in
	    case ns of
		  equal (plus(s1, num n),v2) => if n<0 then equal (standardizeNexp(plus(v2, num (~n))), s1) else ns (*standardizes sides of equal; standardizeNexp is needed for crazy x-n=0 case*)
		| _ => ns
	  end
	
  fun standardizeProgram(p: Program) =
    case p of
	  skip => skip
	| concat (p1,p2) => concat (standardizeProgram(p1),standardizeProgram(p2))
    | assign (x,n) =>  assign (x,standardizeNexp(n))
	| ifThenElse (b,p1,p2) => ifThenElse (standardizeBexp(b),standardizeProgram(p1),standardizeProgram(p2))
	| whileDo (b,p) => whileDo(standardizeBexp(b),standardizeProgram(p))
	  
	  
  fun getVarName(var x) = x
  | getVarName _ = raise Fail "LHS must be a variable"
	  
  fun replaceVarNexp(n: Nexp, toreplace: string, replacement: Nexp) =
	case n of
	  num _ => n
    | var x => if x = toreplace then replacement else var x
    | plus (n1,n2) => plus (replaceVarNexp(n1, toreplace, replacement),replaceVarNexp(n2, toreplace, replacement))
	
  fun replaceVarBexp(b: Bexp, toreplace: string, replacement: Nexp) =
    case b of
	  boolean _ => b
	| imply (b1,b2) => imply (replaceVarBexp(b1, toreplace, replacement),replaceVarBexp(b2, toreplace, replacement))
	| less (n1,n2) => less (replaceVarNexp(n1, toreplace, replacement),replaceVarNexp(n2, toreplace, replacement))
	| equal (n1,n2) => equal (replaceVarNexp(n1, toreplace, replacement),replaceVarNexp(n2, toreplace, replacement))
	  
    
  fun programType(p: Program) =
    case p of skip => "skip"
       | concat p => "comp"
       | assign p => "assign"
       | ifThenElse p => "if"
       | whileDo p => "while"
  
	
  fun weakestPrecondition(p: Program, b: Bexp) =
    case p of skip => b
       | concat p => b
       | assign p => (fn (var x,E) => replaceVarBexp(b, x, E) | _ => boolean false) p
       | ifThenElse p => b
       | whileDo p => b

(*
  fun availableRules(Rule {premises, conclusion}) =
	let 
	  val rules = ["str", "weak", "true", "false"]
	  
	  (*somehow determine if we need to allow and / or based on premises*)
	   val extrarules = case (#2 conclusion) of skip => "skip"
       | concat p => "comp"
       | assign p => "assign"
       | ifThenElse p => "if"
       | whileDo p => "while"
	in
	  extrarules :: rules
	end
  *)

  fun toStringNexp(n: Nexp) =
	case n of 
	  num n => Int.toString(n)
	| var n => n
	| plus (n1,n2) => toStringNexp(n1) ^ " + " ^ toStringNexp(n2)

  fun toStringBexp(b: Bexp) =
	case b of 
	  boolean b => Bool.toString(b)
    | imply (imply (p,imply (q,boolean false)),boolean false) => "(" ^ toStringBexp(p) ^ " & " ^ toStringBexp(q) ^ ")"   (* P ∧ Q *)
    | imply (b1,boolean false) => "~" ^ toStringBexp(b1)  (* ¬P *)
    | imply (imply(p, boolean false),b2) => "(" ^ toStringBexp(p) ^ " | " ^ toStringBexp(b2) ^ ")" (* P ∨ Q *)
    | imply (b1,b2) => "(" ^ toStringBexp(b1) ^ " => " ^ toStringBexp(b2) ^ ")"
	| less (n1,n2) => toStringNexp(n1) ^ " < " ^ toStringNexp(n2)
	| equal (n1,n2) => toStringNexp(n1) ^ " = " ^ toStringNexp(n2)

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

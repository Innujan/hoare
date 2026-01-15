structure Standardization = struct
  open Datatypes
  open Utils
  
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
				if(varsidev = varsidew andalso v2 = w2) then
					if numsw <= numsv then
						boolean true
					else
						(case (standardizeBexp(less(v1,v2)),standardizeBexp(less(plus(w2, num ~1),w1))) of
						(less(x1, x2), less(y1, y2)) => 
							let
								val numsx = getNumPlus(x1)
								val numsy = getNumPlus(y1)
							in
								(if (numsx > numsy) then 
									booland (less(x1, x2), less(y1, y2)) else
									booland (less(y1, y2), less(x1, x2)))
							end
						| _ => boolean false
							)
				else boolean false
			end
		)
		| (less _, equal _) => boolean true
		| (equal (v1, v2), boolean false) => standardizeBexp(boolor(less(v1,v2), less(v2,v1)))
		| (equal (v1, v2), less (w1, w2)) => (
			let
				val varsidev = getVarsLHPlus(v1)
				val numsv = getNumPlus(v1)
				val varsidew = getVarsLHPlus(w1)
				val numsw = getNumPlus(w1)
			in
				if(varsidev = varsidew andalso v2 = w2) then
					if numsw <= numsv then
						boolean true
					else
						standardizeBexp(boolnot(equal(v1,v2)))
				else boolean false
			end
		)
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
	  
end
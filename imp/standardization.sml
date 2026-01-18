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
	  
  fun getVarsLHPlus(v) = case v of
	(plus (s, num n)) => SOME s
	| num n => NONE
	| _ => SOME v
  fun getNumPlus(v) = case v of
	(plus (s, num n)) => n
	| num n => n
	| _ => 0

  fun negLit (less (a,b)) = less (b,a)
    | negLit _ = raise Fail "negLit: non-less literal"

  fun negateClause (c : Region) : DNF =
    map (fn lit => [negLit lit]) c
	
  fun negateDNF (dnf : DNF) : DNF =
	  let
		(* Cartesian product of two DNFs *)
		fun cross (d1 : DNF, d2 : DNF) : DNF =
		  List.concat (map (fn c1 => map (fn c2 => c1 @ c2) d2) d1)

		(* helper to remove duplicate literals within a clause *)
		fun uniqClause clause =
		  let
			fun insert x [] = [x]
			  | insert x (y::ys) =
				  if x = y then y::ys
				  else y :: insert x ys
		  in
			List.foldl (fn (lit, acc) => insert lit acc) [] clause
		  end

		(* remove duplicate clauses from DNF *)
		fun uniqDNF dnf =
		  let
			fun insert c [] = [c]
			  | insert c (c'::rest) =
				  if c = c' then c'::rest
				  else c' :: insert c rest
		  in
			List.foldl (fn (clause, acc) => insert clause acc) [] dnf
		  end
	  in
		case dnf of
			[] => [[]]
		  | _ =>
			  let
				val raw = foldl (fn (clause, acc) => cross (negateClause clause, acc)) [[]] dnf
				val cleanedClauses = List.map uniqClause raw
			  in
				uniqDNF cleanedClauses
			  end
	  end

	
  fun standardizeBexp(b: Bexp) : DNF =
    case b of
	  boolean true => [[]]
	| boolean false => []
    | imply (b1,b2) =>
      let
        val dnf1 = standardizeBexp(b1)
        val dnf2 = standardizeBexp(b2)
      in
        (negateDNF(dnf1)) @ dnf2
      end
	| less (n1,n2) =>
	  let
	    val v1 = standardizeNexp(n1)
		val v2 = standardizeNexp(n2)
	  in
	    [[ less (v1,v2) ]]
	  end
	| equal (n1,n2) =>
	  let
		val v1 = standardizeNexp(n1)
		val v2 = standardizeNexp(n2)
	  in
	    [[ less (standardizeNexp(plus(v1,num (~1))),v2), less (standardizeNexp(plus(v2,num (~1))),v1) ]]
	  end
	
	
  fun standardizeBexpOpt (b: Bexp, maxClauses: int) : DNF option =
      (* SAFE VERSION *)
	  let
		fun std b =
		  case b of
			  boolean true => SOME [[]]
			| boolean false => SOME []
			| imply (b1,b2) =>
				(case (std b1, std b2) of
					(SOME dnf1, SOME dnf2) =>
					  let
						fun sizeCross d1 d2 = List.length d1 * List.length d2
						fun safeCross d1 d2 =
						  if sizeCross d1 d2 > maxClauses then NONE
						  else SOME (List.concat (map (fn c1 => map (fn c2 => c1 @ c2) d2) d1))
					  in
						(case safeCross (map (fn lit => [lit]) (List.concat dnf1)) [[]] of
							NONE => NONE
						  | SOME negatedDnf1 => SOME (negatedDnf1 @ dnf2))
					  end
				  | _ => NONE)
			| less (n1,n2) =>
				SOME [[ less (standardizeNexp n1, standardizeNexp n2) ]]
			| equal (n1,n2) =>
				let
				  val v1 = standardizeNexp n1
				  val v2 = standardizeNexp n2
				in
				  SOME [[ less (plus(v1,num(~1)),v2), less (plus(v2,num(~1)),v1) ]]
				end
	  in
		std b
	  end



	fun varsInNexp n =
	  case getVarsLHPlus n of
		NONE => []
	  | SOME v => flattenVars v

	fun allVarsRegion (r : Region) : string list =
	  let
		val vars = List.concat (List.map (fn
		  less(a,b) => varsInNexp a @ varsInNexp b
		| _ => raise Fail "allVarsRegion: non-less expression"
		) r)

		fun insertSorted x [] = [x]
		  | insertSorted x (y::ys) =
			  if x = y then y::ys
			  else if x < y then x::y::ys
			  else y :: insertSorted x ys

		fun uniqueSorted lst = List.foldl (fn (x, acc) => insertSorted x acc) [] lst
	  in
		uniqueSorted vars
	  end


	fun lessToIneq (varOrder : string list) (less(a,b)) : FourierMotzkin.inequality =
	  let
		val lhsVars = varsInNexp a
		val rhsVars = varsInNexp b

		fun countOcc v lst = List.foldl (fn (x,acc) => if x=v then acc+1 else acc) 0 lst

		val coeffs = List.map (fn v => 
		  let
			val c = countOcc v lhsVars - countOcc v rhsVars
		  in
			FourierMotzkin.C c
		  end
		) varOrder

		val rhs = getNumPlus b - getNumPlus a
	  in
		FourierMotzkin.I { coeffs = coeffs, rhs = rhs }
	  end

	fun regionToFM (r : Region) : FourierMotzkin.inequality list =
	  let
		val vars = allVarsRegion r
	  in
		List.map (fn
		  less(a,b) => lessToIneq vars (less(a,b))
		| _ => raise Fail "regionToFM: non-less expression"
		) r
	  end

	fun isRegionEmpty (r : Region) : bool =
	  FourierMotzkin.isEmpty (regionToFM r)


  fun BexpImplies (A : Bexp, B : Bexp) : bool =
    let
      val dnf = standardizeBexp(booland(A, boolnot(B)))
    in
      List.all (fn (region : Region) => isRegionEmpty region) dnf
    end
	
  fun BexpEqual (A : Bexp, B : Bexp) : bool =
    BexpImplies(A, B) andalso BexpImplies(B, A)
	  
end
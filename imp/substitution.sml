structure Substitution = struct
  open Datatypes
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
	  
end
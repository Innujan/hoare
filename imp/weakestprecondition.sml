structure WeakestPrecondition = struct
  open Datatypes
  open Substitution
	
  fun weakestPrecondition(p: Program, b: Bexp) =
    case p of skip => SOME b
       | concat (p1,p2) => (case weakestPrecondition(p2, b) of
			NONE => NONE
		  | SOME wp2 => weakestPrecondition(p1, wp2))
       | assign p => SOME ((fn (var x,E) => replaceVarBexp(b, x, E) | _ => boolean false) p)
       | ifThenElse (w, p1, p2) => 
			let
				val wp1 = weakestPrecondition(p1, b)
				val wp2 = weakestPrecondition(p2, b)
			in
				case (wp1, wp2) of
					 (SOME w1, SOME w2) => SOME (booland(imply(w, w1), imply(boolnot(w), w2)))
				   | _ => NONE
			end
       | whileDo p => NONE (*non-trivial*)

end
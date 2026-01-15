structure WeakestPrecondition = struct
  open Datatypes
  open Substitution
	
  fun weakestPrecondition(p: Program, b: Bexp) =
    case p of skip => b
       | concat p => b
       | assign p => (fn (var x,E) => replaceVarBexp(b, x, E) | _ => boolean false) p
       | ifThenElse p => b
       | whileDo p => b

end
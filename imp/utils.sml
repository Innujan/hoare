structure Utils = struct
  open Datatypes
  	  
	  
  fun getVarName(var x) = x
  | getVarName _ = raise Fail "LHS must be a variable"
    
  fun programType(p: Program) =
    case p of skip => "skip"
       | concat p => "comp"
       | assign p => "assign"
       | ifThenElse p => "if"
       | whileDo p => "while"
  
  
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
end
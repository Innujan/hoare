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

  fun flattenVars n =
	  let
		fun aux nexp acc =
		  case nexp of
			  var x => x :: acc
			| plus(l,r) => aux l (aux r acc)
			| num _ => acc
	  in
		aux n []
	  end
	  
  
  fun replaceFirst (substr: string, repl: string, str: string) =
    let
        val n = String.size substr
        val m = String.size str

        fun matchAt i =
            let
                fun loop j =
                    if j = n then true
                    else if i+j >= m then false
                    else if String.sub(str, i+j) <> String.sub(substr, j) then false
                    else loop (j+1)
            in
                loop 0
            end

        fun find i =
            if i > m - n then NONE
            else if matchAt i then SOME i
            else find (i+1)
    in
        case find 0 of
            NONE => str
          | SOME i =>
                let
                    val b = String.substring(str, 0, i)
                    val a = String.substring(str, i+n, m - (i+n))
                in
                    b ^ repl ^ a
                end
    end
	
  fun countChar (c: char, s: string) =
    let
        val n = String.size s
        fun loop i acc =
            if i = n then acc
            else if String.sub(s,i) = c then loop (i+1) (acc+1)
            else loop (i+1) acc
    in
        loop 0 0
    end
	
	
  fun getVarsLHPlus(v) = case v of
	(plus (s, num n)) => SOME s
	| num n => NONE
	| _ => SOME v
  fun getNumPlus(v) = case v of
	(plus (s, num n)) => n
	| num n => n
	| _ => 0
end
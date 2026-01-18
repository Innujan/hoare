structure Loop = struct
  open Datatypes
  open Utils
  open Standardization
  open Substitution
  open WeakestPrecondition
  open PrettyPrint
  open Parser

  (*funzione che applica la regola specificata da linea di comando*)
  fun apply (rule: string, StepData{triple=t,rule=r,Q=q}) : StepResult =
    let
	  val c_stepdata = StepData{triple=t,rule=r,Q=q}
	  val completed = StepResult{stepdata=[],message=""}
	in
	if List.exists (fn y => rule = y) ["skip", "assign", "if", "while", "comp"] andalso programType(#2 t) <> rule then StepResult{stepdata=[c_stepdata],message="\"" ^ rule ^ "\" rule cannot be applied because program is of type \"" ^ programType(#2 t) ^ "\"."} else
	  case (rule, t) of
	    ("true", _) =>
		  if (BexpEqual(#3 t, boolean(true))) then completed else StepResult{
			stepdata=[c_stepdata],
			message="\"true\" rule cannot be applied because postcondition is not true."
		  }
	  | ("false", _) =>
		  if (BexpEqual(#1 t, boolean(false))) then completed else StepResult{
			stepdata=[c_stepdata],
			message="\"false\" rule cannot be applied because precondition is not false."
		  }
	  | ("str", _) => 
		(case q of
        NONE =>
            StepResult{
			stepdata=[c_stepdata],
			message="Q not provided."
		  }
		| SOME q =>
            if(BexpImplies(#1 t, q))then
			StepResult{
			  stepdata=[StepData{
				triple=(q,#2 t,#3 t),
				rule=Rule{
					premises = [pimplication (#1 t,q), ptriple (selectedtriple (q, #2 t, #3 t))],
					conclusion = normaltriple t
				},
				Q=NONE
			  }],
			  message=""
		    }
		  else
		  StepResult{
			stepdata=[c_stepdata],
			message="Invalid Q: " ^ toStringBexp(#1 t) ^ " => " ^ toStringBexp(q) ^ " is not true."
		  })
	  | ("weak", _) => 
		(case q of
        NONE =>
            StepResult{
			stepdata=[c_stepdata],
			message="Q not provided."
		  }
		| SOME q =>
            if(BexpImplies(q, #3 t))then
			StepResult{
			  stepdata=[StepData{
				triple=(#1 t,#2 t,q),
				rule=Rule{
					premises = [ptriple (selectedtriple (#1 t, #2 t, q)), pimplication (q,#3 t)],
					conclusion = normaltriple t
				},
				Q=NONE
			  }],
			  message=""
		    }
		  else
		  StepResult{
			stepdata=[c_stepdata],
			message="Invalid Q: " ^ toStringBexp(q) ^ " => " ^ toStringBexp(#3 t) ^ " is not true."
		  })
	  | ("and", (pre, program, post)) => 
		(case getBoolAnd(post) of
		SOME (b1,b2) => StepResult{
			  stepdata=[StepData{ (*aggiunge le premesse del programma and da dimostrare*)
				triple=(pre, program, b1),
				rule=Rule{
					premises = [ptriple (normaltriple (pre, program, b1)), ptriple (selectedtriple (pre, program, b2))],
					conclusion = normaltriple t
				},
				Q=NONE
			  },
			  StepData{
				triple=(pre, program, b2),
				rule=Rule{
					premises = [ptriple (selectedtriple (pre, program, b1)), ptriple (normaltriple (pre, program, b2))],
					conclusion = normaltriple t
				},
				Q=NONE
			  }],
			message=""
		  }
		| NONE => StepResult{
			stepdata=[c_stepdata],
			message="Postcondition is not a sequence of ands."
		  })
	  | ("or",(pre, program, post)) => 
		(case getBoolOr(pre) of
		SOME (b1,b2) => StepResult{
			  stepdata=[StepData{ (*aggiunge le premesse del programma or da dimostrare*)
				triple=(b1, program, post),
				rule=Rule{
					premises = [ptriple (normaltriple (b1, program, post)), ptriple (selectedtriple (b2, program, post))],
					conclusion = normaltriple t
				},
				Q=NONE
			  },
			  StepData{
				triple=(b2, program, post),
				rule=Rule{
					premises = [ptriple (selectedtriple (b1, program, post)), ptriple (normaltriple (b2, program, post))],
					conclusion = normaltriple t
				},
				Q=NONE
			  }],
			message=""
		  }
		| NONE => StepResult{
			stepdata=[c_stepdata],
			message="Precondition is not a sequence of ors."
		  })
	  | ("skip", _) => (*regola SKIP*)
		  if (BexpEqual(#1 t, #3 t)) then (*se vero, allora la precondizione e la postcondizione sono uguali e quindi la tripla corrisponde all'assioma dello skip*)
		    completed
		  else
		    StepResult{
				stepdata=[c_stepdata],
				message="\"skip\" rule cannot be applied because precondition and postcondition do not match.\n\n\u001b[1;36m[Tip] Try using the \"str\" rule with Q = " ^ toStringBexp(case weakestPrecondition(#2 t, #3 t) of SOME wp => wp | _ => boolean(false)) ^ ", the weakest precondition that allows \"skip\" in the next step."
			  }
	  | ("assign", _) => (*assegnamento*)
		(case #2 t of
			assign (var x, E) => if (BexpEqual(#1 t, replaceVarBexp(#3 t, x, E))) then
					completed
				  else
					StepResult{
						stepdata=[c_stepdata],
						message="\"assign\" rule cannot be applied because precondition is not equal to [" ^ (toStringNexp(E)) ^ "/" ^ x ^ "] of postcondition.\n\n\u001b[1;36m[Tip] Try using the \"str\" rule with Q = " ^ toStringBexp(case weakestPrecondition(#2 t, #3 t) of SOME wp => wp | _ => boolean(false)) ^ ", the weakest precondition that allows \"assign\" in the next step."
					  }
			| _ => StepResult{
			stepdata=[c_stepdata],
			message="Invalid assignment."
			}
		  )
	  | ("if", (pre, ifThenElse(ifcond, trueprog, falseprog), post)) => (*regola IF THEN ELSE*)
		let
		  val truepre = booland(pre, ifcond) (*precondizione della tripla con il programma eseguito se la condizione dell'if è vera*)
		  val falsepre = booland(pre, boolnot(ifcond)) (*precondizione della tripla con il programma eseguito se la condizione dell'if è falsa*)
		in
		  StepResult{
			  stepdata=[StepData{ (*aggiunge le premesse del programma if da dimostrare*)
				triple=(falsepre, falseprog, post),
				rule=Rule{
					premises = [ptriple (normaltriple (truepre, trueprog, post)), ptriple (selectedtriple (falsepre, falseprog, post))],
					conclusion = normaltriple t
				},
				Q=NONE
			  },
			  StepData{
				triple=(truepre, trueprog, post),
				rule=Rule{
					premises = [ptriple (selectedtriple (truepre, trueprog, post)), ptriple (normaltriple (falsepre, falseprog, post))],
					conclusion = normaltriple t
				},
				Q=NONE
			  }],
						message=""
					  }
		end
	  | ("while", (pre, whileDo(cond, prog), post)) => (*regola WHILE*)
	    if (BexpEqual(post, booland(pre, boolnot(cond)))) then
		  StepResult{
			  stepdata=[StepData{ (*aggiunge le premesse del programma while da dimostrare*)
				triple=(booland(pre, cond), prog, pre),
				rule=Rule{
					premises = [ptriple (selectedtriple (booland(pre, cond), prog, pre))],
					conclusion = normaltriple t
				},
				Q=NONE
			  }
			  ],
			  message=""}
		  else
		    StepResult{
				stepdata=[c_stepdata],
				message="\"while\" rule cannot be applied because postcondition is not equal to P & ~Q."
			  }
	  | ("comp", _) => (*;*)
	  (case q of
        NONE =>
            StepResult{
			stepdata=[c_stepdata],
			message="Q not provided."
		  }
		| SOME q =>
		(case #2 t of
			concat (a, b) => StepResult{
						stepdata=[StepData{
				triple=(q,b,#3 t),
				rule=Rule{
					premises = [ptriple (normaltriple (#1 t,a,q)), ptriple (selectedtriple (q,b,#3 t))],
					conclusion = normaltriple t
				},
				Q=NONE
			  },
			  StepData{
				triple=(#1 t,a,q),
				rule=Rule{
					premises = [ptriple (selectedtriple (#1 t,a,q)), ptriple (normaltriple (q,b,#3 t))],
					conclusion = normaltriple t
				},
				Q=NONE
			  }],
						message=""
					  }
			| _ => StepResult{
			stepdata=[c_stepdata],
			message="Invalid concatenation."
			}
		  ))
	  | (_, _) => StepResult{
			stepdata=[c_stepdata],
			message="Rule does not exist."
		  }
	end
    (*else
	(*la tripla del programma da dimostrare non corrsiponde a un caso della regola specificata*)
      let
	    val _ = print("rule isn't appliable to current triple\n")
	  in
	    [t]
	  end
*)
  
  (*funzione che chiede all'utente di applicare ricorsivamente le regole*)
  (*d'inferenza sulla tripla in cima a uno stack di triple, dimostrandole ricorsivamente*)
  

  fun requestNexpOperation(pre: string, post: string) =
	let
		val _ = print "Choose a numeric expression:\n"
		val _ = print(String.concat(List.map(fn rule => ("  " ^ rule ^ "  "))(["var", "num", "plus"])))
		val _ = print "\n> "
		val input : string =
			case TextIO.inputLine TextIO.stdIn of
				NONE => ""
			  | SOME s => String.extract(s, 0, SOME (String.size s - 1))
	in
		case input of
		  "var" => 
			let
				val _ = print ("Input the variable name:\n> ")
				val vname : string =
					case TextIO.inputLine TextIO.stdIn of
						NONE => ""
					  | SOME s => String.extract(s, 0, SOME (String.size s - 1))
			in
				case vname of
				  ("back") => NONE
				| _ => SOME ("var(" ^ vname ^ ")")					
			end
		| "num" => 
			let
				val _ = print ("Input the integer value:\n> ")
				val vnum : string =
					case TextIO.inputLine TextIO.stdIn of
						NONE => ""
					  | SOME s => String.extract(s, 0, SOME (String.size s - 1))
			in
				case vnum of
				  ("back") => NONE
				| _ => SOME ("num(" ^ vnum ^ ")")	
			end
		| "plus" => 
				let
					val _ = print ("Input the first (+) operand of " ^ pre ^ "\u001b[1;31m_\u001b[0m + _"^ post ^ ":\n")
					val n1 = requestNexpOperation(pre, " + _ " ^ post)
					val _ = case n1 of SOME n1 => print ("\nInput the second (+) operand of " ^ pre ^ toStringNexp(NexpFromString n1) ^ " + \u001b[1;31m_\u001b[0m" ^ post ^ ":\n") | _ => ()
					val n2 = case n1 of SOME n1 => requestNexpOperation(pre ^ toStringNexp(NexpFromString n1) ^ " + ", post) | _ => NONE
				in
					case (n1, n2) of
					  (SOME n1, SOME n2) => SOME ("plus(" ^ n1 ^ ", " ^ n2 ^ ")")
					| _ => NONE	
				end
		| "back" => NONE
		| _ => 
			let
				val _ = print ("Invalid input!\n")
			in
				requestNexpOperation(pre, post)
			end
	end


  fun requestBexpOperation(pre, post) =
	let
		val _ = print "Choose a boolean expression:\n"
		val _ = print(String.concat(List.map(fn rule => ("  " ^ rule ^ "  "))(["true", "false", "and", "or", "<", "=", geqChar, "imply", "not"])))
		val _ = print "\n> "
		val input : string =
			case TextIO.inputLine TextIO.stdIn of
				NONE => ""
			  | SOME s => String.extract(s, 0, SOME (String.size s - 1))
	in
		case input of
		  "true" => SOME "boolean(true)"
		| "false" => SOME "boolean(false)"
		| "and" => 
			let
				val _ = print ("Input the first operand of the " ^ pre ^ "\u001b[1;31m_\u001b[0m & _"^ post ^ " expression:\n")
				val b1 = requestBexpOperation(pre, " & _ " ^ post)
				val _ = case b1 of SOME b1 => print ("\nInput the second operand of the " ^ pre ^ toStringBexp(BexpFromString b1) ^ " & \u001b[1;31m_\u001b[0m" ^ post ^ " expression:\n") | _ => ()
				val b2 = case b1 of SOME b1 => requestBexpOperation(pre ^ toStringBexp(BexpFromString b1) ^ " & ", post) | _ => NONE
			in
				case (b1, b2) of
				  (SOME b1, SOME b2) => SOME ("and(" ^ b1 ^ ", " ^ b2 ^ ")")
				| _ => NONE
			end
		| "or" => 
			let
				val _ = print ("Input the first operand of the " ^ pre ^ "\u001b[1;31m_\u001b[0m | _"^ post ^ " expression:\n")
				val b1 = requestBexpOperation(pre, " | _ " ^ post)
				val _ = case b1 of SOME b1 => print ("\nInput the second operand of the " ^ pre ^ toStringBexp(BexpFromString b1) ^ " | \u001b[1;31m_\u001b[0m" ^ post ^ " expression:\n") | _ => ()
				val b2 = case b1 of SOME b1 => requestBexpOperation(pre ^ toStringBexp(BexpFromString b1) ^ " | ", post) | _ => NONE
			in
				case (b1, b2) of
				  (SOME b1, SOME b2) => SOME ("or(" ^ b1 ^ ", " ^ b2 ^ ")")
				| _ => NONE
			end
		| "<" => 
			let
				val _ = print ("Input the first operand of the " ^ pre ^ "\u001b[1;31m_\u001b[0m < _"^ post ^ " expression:\n")
				val n1 = requestNexpOperation(pre, " < _ " ^ post)
				val _ = case n1 of SOME n1 => print ("\nInput the second operand of the " ^ pre ^ toStringNexp(NexpFromString n1) ^ " < \u001b[1;31m_\u001b[0m" ^ post ^ " expression:\n") | _ => ()
				val n2 = case n1 of SOME n1 => requestNexpOperation(pre ^ toStringNexp(NexpFromString n1) ^ " < ", post) | _ => NONE
			in
				case (n1, n2) of
				  (SOME n1, SOME n2) => SOME ("less(" ^ n1 ^ ", " ^ n2 ^ ")")
				| _ => NONE				
			end
		| "=" => 
			let
				val _ = print ("Input the first operand of the " ^ pre ^ "\u001b[1;31m_\u001b[0m = _"^ post ^ " expression:\n")
				val n1 = requestNexpOperation(pre, " = _ " ^ post)
				val _ = case n1 of SOME n1 => print ("\nInput the second operand of the " ^ pre ^ toStringNexp(NexpFromString n1) ^ " = \u001b[1;31m_\u001b[0m" ^ post ^ " expression:\n") | _ => ()
				val n2 = case n1 of SOME n1 => requestNexpOperation(pre ^ toStringNexp(NexpFromString n1) ^ " = ", post) | _ => NONE
			in
				case (n1, n2) of
				  (SOME n1, SOME n2) => SOME ("equal(" ^ n1 ^ ", " ^ n2 ^ ")")
				| _ => NONE	
			end
		| ">=" => 
			let
				val _ = print ("Input the first operand of the " ^ pre ^ "\u001b[1;31m_\u001b[0m " ^ geqChar ^ " _"^ post ^ " expression:\n")
				val n1 = requestNexpOperation(pre, " " ^ geqChar ^ " _ " ^ post)
				val _ = case n1 of SOME n1 => print ("\nInput the second operand of the " ^ pre ^ toStringNexp(NexpFromString n1) ^ " " ^ geqChar ^ " \u001b[1;31m_\u001b[0m" ^ post ^ " expression:\n") | _ => ()
				val n2 = case n1 of SOME n1 => requestNexpOperation(pre ^ toStringNexp(NexpFromString n1) ^ " " ^ geqChar ^ " ", post) | _ => NONE
			in
				case (n1, n2) of
				  (SOME n1, SOME n2) => SOME ("not(less(" ^ n1 ^ ", " ^ n2 ^ "))")
				| _ => NONE	
			end
		| "imply" => 
			let
				val _ = print ("Input the antecedent of the " ^ pre ^ "\u001b[1;31m_\u001b[0m => _"^ post ^ " implication:\n")
				val b1 = requestBexpOperation(pre, " => _ " ^ post)
				val _ = case b1 of SOME b1 => print ("\nInput the consequent of the " ^ pre ^ toStringBexp(BexpFromString b1) ^ " => \u001b[1;31m_\u001b[0m" ^ post ^ " implication:\n") | _ => ()
				val b2 = case b1 of SOME b1 => requestBexpOperation(pre ^ toStringBexp(BexpFromString b1) ^ " => ", post) | _ => NONE
			in
				case (b1, b2) of
				  (SOME b1, SOME b2) => SOME ("imply(" ^ b1 ^ ", " ^ b2 ^ ")")
				| _ => NONE
			end
		| "not" => 
			let
				val _ = print ("Input the operand of the " ^ pre ^ "not(\u001b[1;31m_\u001b[0m)"^ post ^ " expression:\n")
				val b1 = requestBexpOperation(pre ^ " not(", ") " ^ post)	
			in
				case b1 of
				  (SOME b1) => SOME ("not(" ^ b1 ^ ")")
				| _ => NONE
			end
		| "back" => NONE
		| _ => 
			let
				val _ = print ("Invalid input!\n")
			in
				requestBexpOperation(pre,post)
			end
	end

  fun readBexp () =
    let
        val line = requestBexpOperation("","")
		val _ = case line of SOME line => print ("\nYou entered: " ^ line ^ "\n") | _ => ()
    in
	    case line of SOME line =>
			SOME (BexpFromString line
			handle _ =>
				( print "Invalid input!\n"; raise Fail "retry" ) )
		| _ => NONE
    end
  
  fun loop (stepdata : StepData list, original_triple : triple, message : string, prev_stepdata : StepData list list) =
    case stepdata of
         [] => (print "\nProof is complete\n"; OS.Process.success) (*stack vuoto: nulla da dimostrare*)
       | (StepData{triple=c_triple,rule=c_rule,Q=_} :: r_stepdata) => (*estrae la cima dello stack*)
           let
			 val _ = print "\u001b[2J\u001b[H"; (*cls*)
			 val _ = print "Original triple:\n"
			 val _ = print ("  " ^ toStringTriple(original_triple) ^ "\n\n")
			 val _ = print "Current derivation step:\n\n"
			 val _ = print (toStringRule(c_rule))
			 
			 val _ = print "\n\n"
			 
		     val _ = if String.size message > 0 then
				print ("\u001b[1;33m" ^ message ^ "\u001b[0m\n\n")
			 else
				()
			 
			 val _ = print "Choose an inference rule to apply:\n"
			 val _ = print(String.concat(List.map(fn rule => ("  " ^ rule ^ "  "))(["true", "false", "str", "weak", "and", "or", "skip", "assign", "if", "while", "comp"])))
			 val _ = (case prev_stepdata of
						  [] => ()
					    | _ => print "\nOr type \"undo\" to undo the previous step")
			 val _ = print "\n> "
			 
			 
           in
             let
               (*legge il valore inserito da linea di comando*)
               val input : string =
							case TextIO.inputLine TextIO.stdIn of
								NONE => ""
							  | SOME s => String.extract(s, 0, SOME (String.size s - 1))
				in 
			      case (input,prev_stepdata) of 
					("undo",stepdata :: prev_stepdata ) => loop(stepdata, original_triple, "Last step was undone.", prev_stepdata)
				  | ("exit",_ ) => OS.Process.exit(OS.Process.success)
				  | _ => let
					val q = if List.exists (fn y => input = y) ["str", "weak"] then
					  let 
						 val _ = print "\u001b[2J\u001b[H"; (*cls*)
						 val _ = print "Original triple:\n"
						 val _ = print ("  " ^ toStringTriple(original_triple) ^ "\n\n")
						 val _ = print "Current derivation step:\n"
						  val _ = print("\n" ^ toStringRule(Rule{
							premises = if input="str" then 	[pdummy (toStringBexp(#1 c_triple) ^ " => %"), pdummy ("{%} " ^ toStringProgram(#2 c_triple) ^ " {" ^ toStringBexp(#3 c_triple) ^ "}")] else 
															[pdummy ("{" ^ toStringBexp(#1 c_triple) ^ "} " ^ toStringProgram(#2 c_triple) ^ " {%}"), pdummy ("% => " ^ toStringBexp(#3 c_triple))],
							conclusion = selectedtriple c_triple
						  }))
						  val _ = print "\n\nInput Q to continue or \"back\" to select a different rule\n\n"
					  in 

						let
							val input = readBexp ()
							val _ = case input of SOME input => print (toStringBexp input) | _ => ()
						in
							input
						end

					  end
				   else if input="comp" andalso programType(#2 c_triple) = "comp" then
					  let
						 val _ = print "\u001b[2J\u001b[H"; (*cls*)
						 val _ = print "Original triple:\n"
						 val _ = print ("  " ^ toStringTriple(original_triple) ^ "\n\n")
						 val _ = print "Current derivation step:\n"
						  val _ = print("\n" ^ toStringRule(Rule{
							premises = case (#2 c_triple) of
							concat (a, b) => [pdummy ("{" ^ toStringBexp(#1 c_triple) ^ "} " ^ toStringProgram(a) ^ " {%}"), pdummy ("{%} " ^ toStringProgram(b) ^ " {" ^ toStringBexp(#3 c_triple) ^ "}")]
							| _ => [],
							conclusion = selectedtriple c_triple
						  }))
						  val _ = print "\n\nInput Q to continue or \"back\" to select a different rule"
						  val _ = case (#2 c_triple) of
							concat (_, b) => (case weakestPrecondition(b, #3 c_triple) of
								SOME wp => print("\n\n\u001b[1;36m[Tip] Try using the weakest precondition of the final program: Q = " ^ toStringBexp(wp) ^ "\u001b[0m")
								| _ => ())
							| _ => ()
						  val _ = print "\n\n"
						  
					  in 

						let
							val input = readBexp ()
							val _ = case input of SOME input => print (toStringBexp input) | _ => ()
						in
							input
						end
					  end
				   else NONE
				 in
				   
				   let
					 val StepResult{stepdata = n_stepdata, message = message} = apply(input, StepData{
						triple=c_triple,rule=c_rule,Q=q
					 }) (*applica la regola d'inferenza specificata per ottenere le premesse della tripla corrente*)
				   in
					 loop (n_stepdata @ r_stepdata, original_triple, message, if message = "" then stepdata :: prev_stepdata else prev_stepdata) (*riesegui la funzione aggiungendo sullo stack le premesse, non ancora dimostrate*)
				   end
				 end
			  end
           end
end

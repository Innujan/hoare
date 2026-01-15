structure Loop = struct
  open Datatypes
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
		  if ((standardizeBexp(#3 t)) = boolean(true)) then completed else StepResult{
			stepdata=[c_stepdata],
			message="\"true\" rule cannot be applied because postcondition is not true."
		  }
	  | ("false", _) =>
		  if ((standardizeBexp(#1 t)) = boolean(false)) then completed else StepResult{
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
            if(standardizeBexp(imply(#1 t, q))=boolean(true))then
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
            if(standardizeBexp(imply(q, #3 t))=boolean(true))then
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
		  (* TODO add AND and OR inference rules!!!!!!!!!!!!!!!!!!!!!! *)
	  | ("skip", _) => (*regola SKIP*)
		  if (standardizeBexp(#1 t) = standardizeBexp(#3 t)) then (*se vero, allora la precondizione e la postcondizione sono uguali e quindi la tripla corrisponde all'assioma dello skip*)
		    completed
		  else
		    StepResult{
				stepdata=[c_stepdata],
				message="\"skip\" rule cannot be applied because precondition and postcondition do not match.\nTry using the \"str\" rule with Q = " ^ toStringBexp(weakestPrecondition(#2 t, #3 t)) ^ ", the weakest precondition that allows \"skip\" in the next step."
			  }
	  | ("assign", _) => (*assegnamento*)
		(case #2 t of
			assign (var x, E) => if (standardizeBexp(#1 t) = standardizeBexp(replaceVarBexp(#3 t, x, E))) then
					completed
				  else
					StepResult{
						stepdata=[c_stepdata],
						message="\"assign\" rule cannot be applied because precondition is not equal to [" ^ (toStringNexp(E)) ^ "/" ^ x ^ "] of postcondition.\nTry using the \"str\" rule with Q = " ^ toStringBexp(weakestPrecondition(#2 t, #3 t)) ^ ", the weakest precondition that allows \"assign\" in the next step."
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
  
  fun readBexp () =
    let
        val line =
            case TextIO.inputLine TextIO.stdIn of
                 NONE => ""
               | SOME s => String.extract (s, 0, SOME (String.size s - 1))
    in
        BexpFromString line
        handle _ =>
            ( print "Invalid input!\n> "; readBexp () )
    end
  
  fun loop (stepdata : StepData list, original_triple : triple, message : string) =
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
				print (message ^ "\n\n")
			 else
				()
			 
			 val _ = print "Choose an inference rule to apply:\n"
			 val _ = print(String.concat(List.map(fn rule => ("  " ^ rule ^ "  "))(["true", "false", "str", "weak", "and", "or", "skip", "assign", "if", "while", "comp"])))
			 val _ = print "\n> "
			 
			 
           in
             let
               (*legge il valore inserito da linea di comando*)
               val input : string =
							case TextIO.inputLine TextIO.stdIn of
								NONE => ""
							  | SOME s => String.extract(s, 0, SOME (String.size s - 1))
				val q = if List.exists (fn y => input = y) ["str", "weak"] then
				  let 
					  val _ = print("\n" ^ toStringRule(Rule{
						premises = if input="str" then 	[pdummy (toStringBexp(#1 c_triple) ^ " => Q"), pdummy ("{Q} " ^ toStringProgram(#2 c_triple) ^ " {" ^ toStringBexp(#3 c_triple) ^ "}")] else 
														[pdummy ("{" ^ toStringBexp(#1 c_triple) ^ "} " ^ toStringProgram(#2 c_triple) ^ " {Q}"), pdummy ("Q => " ^ toStringBexp(#3 c_triple))],
						conclusion = normaltriple c_triple
					  }))
				      val _ = print "\n\nInput Q to continue:\n> "
				  in 

					let
						val input = readBexp ()
						val _ = print (toStringBexp input)
					in
						SOME input
					end

				  end
			   else if input="comp" andalso programType(#2 c_triple) = "comp" then
			      let
					  val _ = case (#2 c_triple) of
						concat (_, b) => print("\nTry using the weakest precondition of the final program: Q = " ^ toStringBexp(weakestPrecondition(b, #3 c_triple)) ^ "\n")
						| _ => ()
					  
					  val _ = print("\n" ^ toStringRule(Rule{
						premises = case (#2 c_triple) of
						concat (a, b) => [pdummy ("{" ^ toStringBexp(#1 c_triple) ^ "} " ^ toStringProgram(a) ^ " {Q}"), pdummy ("{Q} " ^ toStringProgram(b) ^ " {" ^ toStringBexp(#3 c_triple) ^ "}")]
						| _ => [],
						conclusion = normaltriple c_triple
					  }))
				      val _ = print "\n\nInput Q to continue:\n> "
				  in 

					let
						val input = readBexp ()
						val _ = print (toStringBexp input)
					in
						SOME input
					end
				  end
			   else NONE
             in
			   
               let
                 val StepResult{stepdata = n_stepdata, message = message} = apply(input, StepData{
					triple=c_triple,rule=c_rule,Q=q
				 }) (*applica la regola d'inferenza specificata per ottenere le premesse della tripla corrente*)
               in
                 loop (n_stepdata @ r_stepdata, original_triple, message) (*riesegui la funzione aggiungendo sullo stack le premesse, non ancora dimostrate*)
               end
		     end
           end
end

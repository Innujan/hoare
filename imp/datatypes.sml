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
end

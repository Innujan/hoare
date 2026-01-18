structure Main = struct
  open Datatypes
  open Loop
  open Parser

  (*CODICE PER LEGGERE FILE*)
  fun readFile filename =
    let
      val instream = TextIO.openIn filename
      val content = TextIO.inputAll instream
    in
      TextIO.closeIn instream;
      content
    end
	
  fun splitLines s =
    let
      val rawLines = String.tokens (fn c => c = #"\n" orelse c = #"\r") s
    in
      List.filter (fn line => String.size (trim line) > 0) rawLines (*rimuove righe vuote e righe extra causate da parsing \r\n in caso windows*)
    end

  fun tripleFromFile filename =
    let
      val lines = splitLines (readFile filename)
      val lineCount = List.length lines
      val () = if lineCount < 3 then
				  (print ("Input file does not have enough lines\n");
				   raise Fail "too few lines")
               else ()
      val last3 = List.drop (lines, lineCount - 3)
      val line1 = List.nth (last3, 0)
      val line2 = List.nth (last3, 1)
      val line3 = List.nth (last3, 2)

      val bexp1 =
        (BexpFromString (trim line1)) handle Fail msg =>
          (print ("Error parsing precondition: " ^ line1 ^ "\n");
           raise Fail "precondition parsing failed")

      val prog =
        (ProgramFromString (trim line2)) handle Fail msg =>
          (print ("Error parsing program: " ^ line2 ^ "\n");
           raise Fail "program parsing failed")

      val bexp2 =
        (BexpFromString (trim line3)) handle Fail msg =>
          (print ("Error parsing postcondition: " ^ line3 ^ "\n");
           raise Fail "postcondition parsing failed")
    in
      (bexp1, prog, bexp2)
    end

    (*Input integrato da file a parte*)
  fun main (progName, args) =
    let
      val input = 
        case args of
          [] => NONE
        | [filename] => SOME filename
        | _ => raise Fail "invalid arguments"
      val test_triple : triple = tripleFromFile (case input of SOME f => f | NONE => "") handle _ => 
                (print ("Unable to open input file\n"); raise Fail "unable to open input file")

      val stepData = StepData {
        triple = test_triple,
        rule   = Rule { premises = [], conclusion = selectedtriple test_triple },
        Q      = NONE
      }
    in
      loop ([stepData], test_triple, "", []);
      OS.Process.success
    end

end

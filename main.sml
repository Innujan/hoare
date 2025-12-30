structure Main = struct

  (*CODICE PER LEGGERE FILE*)
  (*fun readFile filename =
    let
      val instream = TextIO.openIn filename
      val content = TextIO.inputAll instream
    in
      TextIO.closeIn instream;
      content
    end

  fun processFile filename =
    let
      val source = readFile filename
    in
      print ("using filename: " ^ filename ^ " with contents: " ^ source);
      Parser.parse(source);
      OS.Process.success
    end*)

    open Datatypes
    open Test
    open Loop
    
    (*Input integrato da file a parte*)
  fun main (progName, args) =
    let
      val test_triple = current_goal : triple
      (*PER LETTURA DA FILE: val triple = Datatypes.toStringBexp(#1 Test.current_goal) ^ Datatypes.toStringProgram(#2 Test.current_goal) ^ Datatypes.toStringBexp(#3 Test.current_goal)
       *) in
      (loop([test_triple]); OS.Process.success)
    end

end

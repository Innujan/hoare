structure Main = struct

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

    
  fun main (progName, args) =
    let
      val triple = Datatypes.toStringTriple(Test.current_goal : Datatypes.triple)
      (*val triple = Datatypes.toStringBexp(#1 Test.current_goal) ^ Datatypes.toStringProgram(#2 Test.current_goal) ^ Datatypes.toStringBexp(#3 Test.current_goal)
       *) in
      print ("using triple: " ^ triple);
      OS.Process.success
    end

end

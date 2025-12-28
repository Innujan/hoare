structure Main = struct

  fun readFile filename =
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
    end

    
  fun main (progName, args) =
      case args of
        [] => (print "usage: prover <filename.imp>\n"; OS.Process.failure)
      | [filename] =>
        (processFile filename; OS.Process.success)
      | _ => (print "error: too many arguments.\n"; OS.Process.failure)

end

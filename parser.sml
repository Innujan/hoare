structure Parser = struct

  fun parse(source: string) =
    let
      val parts = String.tokens(fn c => c = #"\n")source
      val pre = List.nth(parts, 0)
      val program = List.nth(parts, 1)
      val post = List.nth(parts, 2)
      val pretkns = boolLex(String.explode phi)
      val prestr = parseBool(pretkns)
    in
      print("precondition: " ^ pre ^ "\nprogram: " ^ program ^ "\npostcondition: " ^ post ^ "\n");
      OS.Process.success
    end

  fun parseBool() =

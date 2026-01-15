structure Parser = struct
    open Datatypes
	
	fun trim s =
		String.implode (List.filter (fn c => not (Char.isSpace c)) (String.explode s))

	fun parseInt s = 
		case Int.fromString s of
			SOME n => n
		  | NONE => raise Fail ("Invalid integer: " ^ s)

	fun splitOnce (c, s) =
		let
			fun aux [] _ _ = NONE
			  | aux (x::xs) idx depth =
					if x = #"(" then aux xs (idx+1) (depth+1)
					else if x = #")" then aux xs (idx+1) (depth-1)
					else if x = c andalso depth = 0 then SOME idx
					else aux xs (idx+1) depth
		in
			case aux (String.explode s) 0 0 of
				NONE => (s, "")
			  | SOME idx =>
					let
						val left = String.substring(s, 0, idx)
						val right = String.substring(s, idx+1, String.size s - idx - 1)
					in
						(left, right)
					end
		end

	fun startsWith (prefix, s) =
		let
			val plen = String.size prefix
		in
			plen <= String.size s andalso String.substring(s, 0, plen) = prefix
		end

	fun endsWith (suffix, s) =
		let
			val slen = String.size suffix
			val slen2 = String.size s
		in
			slen <= slen2 andalso String.substring(s, slen2 - slen, slen) = suffix
		end

	fun NexpFromString s =
		let
			val s = trim s
		in
			if startsWith ("plus(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 5, String.size s - 6)
					val (left, right) = splitOnce (#",", inner)
				in
					plus(NexpFromString (trim left), NexpFromString (trim right))
				end
			else if startsWith ("num(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 4, String.size s - 5)
				in
					num(parseInt (trim inner))
				end
			else if startsWith ("var(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 4, String.size s - 5)
				in
					var(trim inner)
				end
			else
				raise Fail ("Invalid Nexp: " ^ s)
		end

	fun BexpFromString s =
		let
			val s = trim s
		in
			if startsWith ("boolean(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 8, String.size s - 9)
				in
					case trim inner of
						"true" => boolean true
					  | "false" => boolean false
					  | _ => raise Fail ("Invalid boolean: " ^ inner)
				end
			else if startsWith ("imply(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 6, String.size s - 7)
					val (left, right) = splitOnce (#",", inner)
				in
					imply(BexpFromString (trim left), BexpFromString (trim right))
				end
			else if startsWith ("less(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 5, String.size s - 6)
					val (left, right) = splitOnce (#",", inner)
				in
					less(NexpFromString (trim left), NexpFromString (trim right))
				end
			else if startsWith ("equal(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 6, String.size s - 7)
					val (left, right) = splitOnce (#",", inner)
				in
					equal(NexpFromString (trim left), NexpFromString (trim right))
				end
			else if startsWith ("not(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 4, String.size s - 5)
				in
					boolnot(BexpFromString (trim inner))
				end
			else if startsWith ("and(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 4, String.size s - 5)
					val (left, right) = splitOnce (#",", inner)
				in
					booland(BexpFromString (trim left), BexpFromString (trim right))
				end
			else if startsWith ("or(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 3, String.size s - 4)
					val (left, right) = splitOnce (#",", inner)
				in
					boolor(BexpFromString (trim left), BexpFromString (trim right))
				end
			else
				raise Fail ("Invalid Bexp: " ^ s)
		end
		
	fun ProgramFromString s =
		let
			val s = trim s
		in
			if s = "skip" orelse s = "skip()" then
				skip

			else if startsWith ("assign(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 7, String.size s - 8)
					val (lhs, rhs) = splitOnce (#",", inner)
				in
					assign (NexpFromString (trim lhs), NexpFromString (trim rhs))
				end

			else if startsWith ("comp(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 5, String.size s - 6)
					val (p1, p2) = splitOnce (#",", inner)
				in
					concat (ProgramFromString (trim p1), ProgramFromString (trim p2))
				end

			else if startsWith ("if(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 3, String.size s - 4)
					val (bpart, rest) = splitOnce (#",", inner)
					val (p1, p2) = splitOnce (#",", rest)
				in
					ifThenElse (BexpFromString (trim bpart), ProgramFromString (trim p1), ProgramFromString (trim p2))
				end

			else if startsWith ("while(", s) andalso endsWith (")", s) then
				let
					val inner = String.substring(s, 6, String.size s - 7)
					val (bpart, ppart) = splitOnce (#",", inner)
				in
					whileDo (BexpFromString (trim bpart), ProgramFromString (trim ppart))
				end

			else
				raise Fail ("Invalid Program: " ^ s)
		end
end
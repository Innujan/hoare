structure FourierMotzkin =
struct
  datatype coeff = C of int
  datatype inequality = I of { coeffs : coeff list, rhs : int }

  fun int_of_coeff (C x) = x

  (* Add two inequalities *)
  fun add_ineq (I {coeffs = cs1, rhs = k1}, I {coeffs = cs2, rhs = k2}) =
    I { coeffs = ListPair.map (fn (C a, C b) => C (a + b)) (cs1, cs2),
        rhs = k1 + k2 }

  (* Scale inequality by integer *)
  fun scale_ineq s (I {coeffs, rhs}) =
    I { coeffs = List.map (fn C x => C (s * x)) coeffs, rhs = s * rhs }

  (* Last coefficient of inequality *)
  fun last_coeff (I {coeffs, ...}) =
    case List.rev coeffs of
      [] => 0
    | C c :: _ => c

  (* Remove last coefficient *)
  fun remove_last (I {coeffs, rhs}) =
    I { coeffs = List.take (coeffs, length coeffs - 1), rhs = rhs }

  (* Fourier–Motzkin elimination of last variable *)
  fun eliminate_var inequalities =
    let
      (* Split inequalities *)
      val (pos, neg, zero) =
        List.foldl
          (fn (ineq, (p,n,z)) =>
             let val c = last_coeff ineq
             in
               if c > 0 then (ineq::p, n, z)
               else if c < 0 then (p, ineq::n, z)
               else (p, n, ineq::z)
             end)
          ([], [], [])
          inequalities

      (* Combine pos upper bounds with neg lower bounds *)
      fun combine i j =
        let
          val c_i = last_coeff i
          val c_j = last_coeff j
          (* scale so last variable cancels *)
          val scaled_i = scale_ineq (abs c_j) i
          val scaled_j = scale_ineq (abs c_i) j
          val combined = add_ineq (scaled_i, scaled_j)
        in
          remove_last combined
        end
    in
      (* New inequalities from combining pos × neg *)
      List.concat (List.map (fn i => List.map (fn j => combine i j) neg) pos)
      @ List.map remove_last zero
    end

  (* Check emptiness of system *)
  fun isEmpty inequalities =
    let
      fun elim ineqs =
        if ineqs = [] then false
        else
          case List.hd ineqs of
            I {coeffs = [], rhs} =>
              (* Only constants left: system is empty if any rhs <= 0 *)
              List.exists (fn I {coeffs = [], rhs} => rhs <= 0 | _ => false) ineqs
          | _ => elim (eliminate_var ineqs)
    in
      elim inequalities
    end
end

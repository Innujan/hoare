structure Test = struct
  open Datatypes

  (*PROGRAMMA: t:=x; x:=y; y:=t*)
val swap_program = 
    concat(
        concat(
			assign(var "t", var("x")),
            assign(var "x", var("y"))
		),
        assign(var "y", var("t"))
    )

    (*da modificare(?): l'and è stato scritto derivando dall'implicazione e dalle
     costanti booleane*)
    (*PRECONDIZIONI: x = A /\ y = B*)
val pre = imply(imply(equal(var("x"), var("A")), boolean(false)), (equal(var("y"), var("B"))))

    (*POSTCONDIZIONI: x = B /\ y = A*)
val post = imply(imply(equal(var("x"), var("B")), boolean(false)), (equal(var("y"), var("A"))))

val current_goal = (booland(equal(var "x", var "x0"), equal(var "y", var "y0")), swap_program, booland(equal(var "x", var "y0"), equal(var "y", var "x0")))

(*val current_goal = (boolean(false), concat(skip,skip), boolean(true))*)
(*val current_goal = (equal(var("x"), num(2)), assign(var("x"),plus(var("x"), num(1))), equal(var("x"), num(3)))*)

(*val current_goal = (pre, swap_program, post)*)
end

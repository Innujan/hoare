structure Test = struct
  open Datatypes

  (*PROGRAMMA: t:=x; x:=y; y:=t*)
val swap_program = 
    concat(
        assign("t", var("x")),
        concat(
            assign("x", var("y")),
            assign("y", var("t"))
        )
    )

    (*da modificare(?): l'and è stato scritto derivando dall'implicazione e dalle
     costanti booleane*)
    (*PRECONDIZIONI: x = A /\ y = B*)
val pre = imply(imply(equal(var("x"), var("A")), boolean(false)), (equal(var("y"), var("B"))))

    (*POSTCONDIZIONI: x = B /\ y = A*)
val post = imply(imply(equal(var("x"), var("B")), boolean(false)), (equal(var("y"), var("A"))))

val current_goal = (pre, skip, pre)
(*val current_goal = (pre, swap_program, post)*)
end

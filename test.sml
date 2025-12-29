structure Test = struct
val swap_program = 
    Datatypes.concat(
        Datatypes.assign("t", Datatypes.var("x")),
        Datatypes.concat(
            Datatypes.assign("x", Datatypes.var("y")),
            Datatypes.assign("y", Datatypes.var("t"))
        )
    )

val pre = Datatypes.imply(Datatypes.imply(Datatypes.equal(Datatypes.var("x"), Datatypes.var("A")), Datatypes.boolean(false)), (Datatypes.equal(Datatypes.var("y"), Datatypes.var("B"))))
val post = Datatypes.imply(Datatypes.imply(Datatypes.equal(Datatypes.var("x"), Datatypes.var("B")), Datatypes.boolean(false)), (Datatypes.equal(Datatypes.var("y"), Datatypes.var("A"))))

val current_goal = (pre, swap_program, post)
end

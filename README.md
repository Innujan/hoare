# Description
CLI proof assistant for Hoare logic in SML

# Compile and Execute
(Requires SMLNJ)
```
sml make.sml
```
This will produce as output `hoare.[architecture details]`. To execute launch
```
sml @SMLload=hoare.[architecture details] "$@"
```

Use the provided file `test.sml` to define the input of the program, using the constructors of boolean expressions and *Imp* programs, that you can find in the `datatypes.sml` file.

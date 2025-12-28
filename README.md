# Description
CLI proof assistant for Hoare logic in SML

# Compile and Execute
(Requires SMLNJ)
```
sml make.sml
```
This will produce as output `hoare.[architecture details]`. To execute launch
```
sml @SMLload=hoare.[architecture details] "$@" <input_file>
```

The input file should contain the structure of a Hoare Triple in the following exact structure
```
[PRECONDITIONS]
[PROGRAM]
[POSTCONDITIONS]
```
- Preconditions and postconditions must be boolean formulae
- Programs must be written in the *Imp* abstract syntax (skip, assignment, concat, if then else and while)

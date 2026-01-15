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

Input triple is defined in input.txt file on root folder of source directory. The file must have at least three lines, and the final three lines are used to input the desired triple into the program.
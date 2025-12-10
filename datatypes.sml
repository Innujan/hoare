datatype Nexp = num of int | var of string | plus of Nexp * Nexp
datatype Bexp = truth of bool | imply of Bexp * Bexp | less of Nexp * Nexp | equal of Nexp * Nexp
datatype Program = skip | concatenate of Program * Program | assign
  of Nexp * Nexp | ifThenElse of Bexp * Program * Program | whileDo of Bexp *
  Program
datatype Triple = triple of Bexp * Program * Bexp

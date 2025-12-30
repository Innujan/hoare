structure Loop = struct
  open Datatypes

  (*funzione che verifica a quale tipo di programma corrisponde l'input p*)
  fun verify(p: Program) : int =
    case p of skip => 1
       | concat p => 2
       | assign p => 3
       | ifThenElse p => 4
       | whileDo p => 5
    
  (*funzione che applica la regola specificata da linea di comando*)
  fun apply (n: int, t: triple) : triple list =
    if (n > 0) andalso (n < 6) then (*se vero, allora la regola rientra tra quelle dei programmi*)
      if (n = verify(#2 t)) then (*se vero, allora la regola scelta è quella giusta per il programma da dimostrare*)
        case (n, t) of
             (1, _) => (*regola SKIP*)
             if ((#1 t) = (#3 t)) then (*se vero, allora la precondizione e la postcondizione sono uguali e quindi la tripla corrisponde all'assioma dello skip*)
               [] 
             else
               [t]
           | (2, _) => (*regola CONCATENAZIONE DI PROGRAMMI*)
               (*richiede la condizione intermedia oppure genera la weakest precondition*) 
               [t]
         | (3, _) => (*regola ASSEGNAMENTO*)
             (*verifica se la condizione va bene, oppure genera la weakest precondition*)
             [t]
         | (4, (pre, ifThenElse(ifcond, trueprog, falseprog), post)) => (*regola IF THEN ELSE*)
             let
               val truepre = booland(pre, ifcond) (*precondizione della tripla con il programma eseguito se la condizione dell'if è vera*)
               val falsepre = booland(pre, boolnot(ifcond)) (*precondizione della tripla con il programma eseguito se la condizione dell'if è falsa*)
             in
               [(truepre, trueprog, post), (falsepre, falseprog, post)] (*aggiunge le premesse del programma if da dimostrare*)
             end
         | (5, _) =>
             (*da implementare: verificare che la precondizione A è*)
             (*un'invariante e che A /\ notB è la postcondizione, dove B è la condizione del while*)
             [t]
         | (_, _) => [t]
      else
        (*la tripla del programma da dimostrare non corrsiponde a un caso della regola specificata*)
       let
          val _ = print("rule isn't appliable to current triple\n")
        in
          [t]
        end
    else
      (*numero inserito non ha una regola corrispondente*)
      let
          val _ = print("unknown rule\n")
      in
        [t]
      end

  (*funzione che chiede all'utente di applicare ricorsivamente le regole*)
  (*d'inferenza sulla tripla in cima a uno stack di triple, dimostrandole ricorsivamente*)
  fun loop (triples : triple list) =
    case triples of
         [] => (print "proof complete\n"; OS.Process.success) (*stack vuoto: nulla da dimostrare*)
       | (c_triple :: r_triples) => (*estrae la cima dello stack*)
           let
             val _ = print ("current triple to prove is " ^  toStringTriple(c_triple) ^ "\n")
             val _ = print ("insert 1-5 to apply inference rule (1 = skip, 2 = sequence of programs, 3 = assignment, 4 = if then else, 5 = while) or 0 to quit\n")
           in
             let
               (*legge il valore inserito da linea di comando*)
               val input = valOf(TextIO.inputLine TextIO.stdIn)
               val input_n = valOf(Int.fromString(String.substring(input, 0, size input
             - 1)))
           in
             if (input_n = 0) then (*valore inserito = 0 allora termina il programma*)
               (print "exiting\n" ; OS.Process.exit OS.Process.success)
             else
               let
                 val premises = apply(input_n, c_triple) (*applica la regola d'inferenza specificata per ottenere le premesse della tripla corrente*)
               in
                 loop (premises @ r_triples) (*riesegui la funzione aggiungendo sullo stack le premesse, non ancora dimostrate*)
               end
             end
      end
end

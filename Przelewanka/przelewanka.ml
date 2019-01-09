(* Przelewanka             *)
(* Autor: Michał Siennicki *)

let kol = Queue.create ()

let mozliwe_ruchy (woda, ile_ruchow) pojemnosci =

(*print_int ile_ruchow;
  print_string " mozliwe_ruchy\n";
  *)

  for i = 0 to Array.length woda - 1 do
    for j = 0 to Array.length woda - 1 do
      let ile_leje = min woda.(i) (pojemnosci.(j) - woda.(j)) in
      woda.(i) <- woda.(i) - ile_leje;
      woda.(j) <- woda.(j) + ile_leje;
      Queue.add (Array.copy woda, ile_ruchow+1) kol;
      woda.(i) <- woda.(i) + ile_leje;
      woda.(j) <- woda.(j) - ile_leje;
    done;
    
    let tmp = woda.(i) in
    woda.(i) <- 0;
    Queue.add (Array.copy woda, ile_ruchow+1) kol;
    woda.(i) <- pojemnosci.(i);
    Queue.add (Array.copy woda, ile_ruchow+1) kol;
    woda.(i) <- tmp
  done

  

(* (int * int) array -> int *)

let przelewanka tab =
  let szukane    = Array.init (Array.length tab) (fun x -> snd tab.(x)) in
  let pojemnosci = Array.init (Array.length tab) (fun x -> fst tab.(x)) in
  let stan = ref (Array.make (Array.length tab) 0, 0 ) in
  
  print_string "Przelewanka\n";

  while fst !stan <> szukane do
    print_int (fst !stan).(0) ;
    print_int (fst !stan).(1) ;
    print_string " <- tyle wody\n";
    print_int (Queue.length kol);
    print_string " ziomkow na kolejce\n";
    
    
    
    mozliwe_ruchy !stan pojemnosci;

    stan := Queue.take kol
  done;
  snd !stan
    

(* Przelewanka             *)
(* Autor: Michał Siennicki *)
(*  403: ... 0.47 *)

let sprawdzone_stany = Hashtbl.create 100000 (* California, Texas*)
let kol = Queue.create ()

let dodaj_na_kol woda ile_ruchow =

    
  let woda = Array.copy woda in
  if false = Hashtbl.mem sprawdzone_stany woda then
  begin
    Hashtbl.add sprawdzone_stany woda 1;
    
    if (Hashtbl.length sprawdzone_stany) mod 100 = 1 then
    begin
 
    
    print_int( Hashtbl.length sprawdzone_stany);
    print_string " elementow w Hashtbl  ";
    print_int( ile_ruchow);
    print_string " ruchow\n"
   
    end;

    
    Queue.add (woda, ile_ruchow) kol
  end
  
let mozliwe_ruchy (woda, ile_ruchow) pojemnosci =
(*
print_int ile_ruchow;
print_string " ruchow\n";
  *)
  for i = 0 to Array.length woda - 1 do
    for j = 0 to Array.length woda - 1 do
      let ile_leje = min woda.(i) (pojemnosci.(j) - woda.(j)) in
      if ile_leje > 0 then
      begin
        woda.(i) <- woda.(i) - ile_leje;
        woda.(j) <- woda.(j) + ile_leje;
        dodaj_na_kol woda (ile_ruchow + 1);
        woda.(i) <- woda.(i) + ile_leje;
        woda.(j) <- woda.(j) - ile_leje
      end
    done;
    
    let tmp = woda.(i) in
    if tmp <> 0 then
    begin
      woda.(i) <- 0;
      dodaj_na_kol woda (ile_ruchow + 1)
    end;
    
    if tmp <> pojemnosci.(i) then
    begin
      woda.(i) <- pojemnosci.(i);
      dodaj_na_kol woda (ile_ruchow + 1)
    end;
    
    woda.(i) <- tmp
  done

let przelewanka tab =
  Hashtbl.clear sprawdzone_stany;
  Queue.clear kol;
  let szukane    = Array.init (Array.length tab) (fun x -> snd tab.(x)) in
  let pojemnosci = Array.init (Array.length tab) (fun x -> fst tab.(x)) in
  let stan = ref (Array.make (Array.length tab) 0, 0 ) in
  
  try
  
  while fst !stan <> szukane do
(*    print_int (compare (fst !stan)  szukane) ;
    print_string " <- tyle wody\n";
  *)
  (*
    print_int (fst !stan).(0) ;
    print_int (fst !stan).(1) ;
    print_string " <- tyle wody\n";
    print_int (Queue.length kol);
    print_string " ziomkow na kolejce\n";
  *)
    mozliwe_ruchy !stan pojemnosci;

    stan := Queue.take kol
  done;
  snd !stan
  with Queue.Empty -> -1

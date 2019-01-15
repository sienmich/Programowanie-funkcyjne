(* Przelewanka             *)
(* Autor: Michał Siennicki *)

let sprawdzone_stany = Hashtbl.create 100000
let kol = Queue.create ()
let znalazl = ref (-1)
let szukane = ref [||]

let dodaj_na_kol woda ile_ruchow =
  let woda = Array.to_list woda in
  if false = Hashtbl.mem sprawdzone_stany woda then
  begin
    Hashtbl.add sprawdzone_stany woda 0;
    let l = Array.of_list woda in
    if l = !szukane then begin znalazl := ile_ruchow end;
    Queue.add (l, ile_ruchow) kol
  end
  
let mozliwe_ruchy (woda, ile_ruchow) pojemnosci =
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

let przelewanko tab =
  Hashtbl.clear sprawdzone_stany;
  Queue.clear kol;
  znalazl := -1;
  szukane := Array.init (Array.length tab) (fun x -> snd tab.(x));
  let pojemnosci = Array.init (Array.length tab) (fun x -> fst tab.(x)) in
  dodaj_na_kol (Array.make (Array.length tab) 0) 0;
  let stan = ref (Queue.take kol) in
  
  try
  while !znalazl = -1 do
    mozliwe_ruchy !stan pojemnosci;
    stan := Queue.take kol
  done;
  !znalazl
  with Queue.Empty -> -1
  
let optymalizacja_pelno_pusta tab =
  let wyn = ref true in
  let pom (a, b) = b = 0 || a = b in
  for i = 0 to -1 + Array.length tab do
    if pom tab.(i) then begin wyn := false end; 
  done;
  !wyn
  
let optymalizacja_nwd tab =
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b) in
  let nwd = max 1 (Array.fold_left (fun x (a, _) -> gcd x a) 0 tab) in
  Array.fold_left (fun x (_, b) -> x || (b mod nwd > 0)) false tab

let przelewanka tab =
  (* testy optymalizacji "choc jeden pelny/pusty" *)
  if optymalizacja_pelno_pusta tab && Array.length tab > 0 then -1 else

  (* testy optymalizacji na -1, sprawdzanie nwd *)
  if optymalizacja_nwd tab then -1 else
  
  przelewanko tab

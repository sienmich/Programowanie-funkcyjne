(* Przelewanka             *)
(* Autor: Michał Siennicki *)
(* Review: *)

(* Rozwiązanie polega na sprawdzaniu wszystkich możliwych stanów,
   gdzie stan to ilość wody w kolejnych pojemnikach.
   Robię to w kolejności rosnącej liczby ruchów *)

(* Hash tablica, w której pamiętam odwiedzone stany *)
let sprawdzone_stany = Hashtbl.create 100000

(* Kolejka, na którą wrzucam stany do sprawdzenia w kolejności rosnącej liczby ruchów *)
let kol = Queue.create ()

(* Jeśli algorytm znajdzie rozwiązanie, to jego wartość to wynik *)
let wynik = ref (-1)

(* Stan, który chcę osiągnąć, czyli podawany do funkcji przelewanka *)
let szukany_stan = ref [||]

(* Funkcja, która dodaje nowy stan na kolejkę do przeanalizowania,
   jeśli wcześniej nie był on przetworzony *)
let dodaj_na_kol stan ile_ruchow =
  let stan = Array.to_list stan in
  if not (Hashtbl.mem sprawdzone_stany stan) then
  begin
    Hashtbl.add sprawdzone_stany stan 0;
    let l = Array.of_list stan in
    if l = !szukany_stan then wynik := ile_ruchow;
    Queue.add (l, ile_ruchow) kol
  end
  
(* Funkcja, która z danego stanu generuje kolejne stany
   wykonując wszystkie możliwe ruchy *)
let mozliwe_ruchy (stan, ile_ruchow) pojemnosci =
  for i = 0 to Array.length stan - 1 do
    let tmp = stan.(i) in

    if tmp <> 0 then
    begin
      stan.(i) <- 0;
      dodaj_na_kol stan (ile_ruchow + 1)
    end;
    
    if tmp <> pojemnosci.(i) then
    begin
      stan.(i) <- pojemnosci.(i);
      dodaj_na_kol stan (ile_ruchow + 1)
    end;
    
    stan.(i) <- tmp;
    
    for j = 0 to Array.length stan - 1 do
      let ile_leje = min stan.(i) (pojemnosci.(j) - stan.(j)) in
      if ile_leje > 0 then
      begin
        stan.(i) <- stan.(i) - ile_leje;
        stan.(j) <- stan.(j) + ile_leje;
        dodaj_na_kol stan (ile_ruchow + 1);
        stan.(i) <- stan.(i) + ile_leje;
        stan.(j) <- stan.(j) - ile_leje
      end
    done
  done
  
(* Główny algorytm, który wykonuje kolejne ruchy
   dopóki nie znajdzie rozwiązania *)
let backtrack tab =
  Hashtbl.clear sprawdzone_stany;
  Queue.clear kol;
  wynik := -1;
  szukany_stan := Array.init (Array.length tab) (fun x -> snd tab.(x));
  let pojemnosci = Array.init (Array.length tab) (fun x -> fst tab.(x)) in
  dodaj_na_kol (Array.make (Array.length tab) 0) 0;
  let stan = ref (Queue.take kol) in
  try
    while !wynik = -1 do
      mozliwe_ruchy !stan pojemnosci;
      stan := Queue.take kol
    done;
    !wynik
  with Queue.Empty -> -1
  
(* Optymalizacja sprawdzająca czy wszystkie pojemniki są nie pełne i nie puste *)
let optymalizacja_pelno_pusta tab =
  let niepusty_niepelny (a, b) = b > 0 && a > b in
  Array.fold_left (fun p i -> p && niepusty_niepelny i) true tab
  
(* Optymalizacja sprawdzająca czy któraś szukana ilość wody
   jest niepodzielna przez nwd pojemności pojemników *)
let optymalizacja_nwd tab =
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b) in
  let nwd = max 1 (Array.fold_left (fun wyn (a, _) -> gcd wyn a) 0 tab) in
  Array.fold_left (fun wyn (_, b) -> wyn || (b mod nwd > 0)) false tab

(* Funkcja, która po sprawdzeniu optymalizacji
   uruchamia główny algorytm przeszukiwania stanów *)
let przelewanka tab =
  if optymalizacja_pelno_pusta tab && Array.length tab > 0 then -1 else
  if optymalizacja_nwd tab then -1 else
  backtrack tab

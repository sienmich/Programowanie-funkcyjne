(* Autor - Michał Siennicki *)

(** Sortowanie topologiczne *)

(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne


(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)
(* ('a * 'a list) list -> 'a list *)

let dodaj komu ile mapa zerowi =
  let ile = ile + (try find komu mapa with Not_found -> 0) in
  if ile = 0
  then (add komu ile mapa, komu::zerowi)
  else (add komu ile mapa, zerowi)

let init lis =
  let f mapa (ele, lista) =
    List.fold_left (fun m e -> fst (dodaj e 1 m []) ) mapa lista in
  List.fold_left f empty lis

let mapa_kolejnosci lis =
  let f mapa (ele, lista) =
    add ele lista mapa in
  List.fold_left f empty lis
  
let init2 lis =
  let m = init lis in
  let f zer (ele, lista) =
    snd (dodaj ele 0 m zer) in
  (m, List.fold_left f [] lis)

(* zwraca (ile_wchodzi, kto_ma_0)  *)
let usuwanko kolejnosc (ile_wchodzi, kto_ma_0) usuwany =
   let lis = try find usuwany kolejnosc with Not_found -> [] in
   let pom (ile_wchodzi, kto_ma_0) komu_zmiejsz =
     dodaj komu_zmiejsz (-1) ile_wchodzi kto_ma_0 in
   List.fold_left pom (ile_wchodzi, kto_ma_0) lis
  
let topol lis =
  let kolejnosc = mapa_kolejnosci lis in
  let (ile_wchodzi, kto_ma_0) = init2 lis in
  let rec f (ile_wchodzi, kto_ma_0)  = 
    match kto_ma_0 with
    | h::t -> 
      h :: (f (usuwanko kolejnosc (ile_wchodzi, t) h))

    | _ when fold (+) ile_wchodzi 0 = 0 -> []
    | _ -> raise Cykliczne in
  f (ile_wchodzi, kto_ma_0)
(*

topol [(1, [2]); (2, [3]); (3, [2])];;

*)
(* val topol lis = *)
  
  
  
  
  
  
  
  
  
  
  
  
  
  

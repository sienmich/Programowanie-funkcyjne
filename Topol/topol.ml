(* Sortowanie topologiczne  *)
(* Autor - Michał Siennicki *)
(* Review - Piotr Jasiński  *)

open PMap

(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne

(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)
let topol lis =
  (* Mapa, która konkretnemu a_i przyporządkowywuje listę elementów,
     które muszą być po nim w kolejności topologicznej *)
  let kolejnosc =
    let f mapa (ele, lista) =
      add ele ((try find ele mapa with Not_found -> []) @ lista) mapa in
    List.fold_left f empty lis in
  
  (* Zwiększa lub zmiejsza licznik krawędzi wchodzących
     do danego wierzchołka grafu *)
  let dodaj komu ile_dodac mapa kto_ma_0 =
    let ile = ile_dodac + (try find komu mapa with Not_found -> (-5)) in
    if ile < (-2) then (add komu ile_dodac mapa, komu::kto_ma_0) else
    if ile = 0 && ile_dodac = (-1)
    then (add komu ile mapa, komu::kto_ma_0)
    else (add komu ile mapa, kto_ma_0) in

  (* Mapa służąca jako licznik krawędzi wchodzących do wierzchołka grafu
     i lista wierzchołków, do których nie wchodzą nieprzetworzone krawędzie *)
  let (ile_wchodzi, kto_ma_0) = 
    let g mapa (ele, lista) =
      List.fold_left (fun m e -> fst (dodaj e 1 m [])) mapa lista in
    let m = List.fold_left g empty lis in
    let f (m, zer) (ele, lista) =
      dodaj ele 0 m zer in
    List.fold_left f (m, []) lis in

  (* Usuwa z grafu wierzchołek, do którego nie wchodzą żadne krawędzie.
     Zwraca zaktualizuje (ile_wchodzi, kto_ma_0) *)
  let usuwanko (ile_wchodzi, kto_ma_0) usuwany =
    let lis = try find usuwany kolejnosc with Not_found -> [] in
    let pom (ile_wchodzi, kto_ma_0) komu_zmiejsz =
      dodaj komu_zmiejsz (-1) ile_wchodzi kto_ma_0 in
    List.fold_left pom (ile_wchodzi, kto_ma_0) lis in
  
  (* Główny algorytm, który po kolei usuwa wierzchołki 
     o zerowym stopniu wchodzącym. Zwraca kolejność topologiczną *)
  let rec f (ile_wchodzi, kto_ma_0)  = 
    match kto_ma_0 with
    | h::t -> h::(f (usuwanko (ile_wchodzi, t) h))
    | _ when fold (+) ile_wchodzi 0 = 0 -> []
    | _ -> raise Cykliczne in
  f (ile_wchodzi, kto_ma_0)
  

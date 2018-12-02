(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz, Michał Siennicki
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Interval Set.
 *
 *  This is an interval set, i.e. a set of integers, where large
 *  intervals can be stored as single elements. Intervals stored in the
 *  set are disjoint. 
 *)
 
(**Typ zbioru przedziałów oparty na drzewach AVL,
 * w których różnica wysyokości poddrzew jest zawsze <= 2
 * Wszystkie przedziały są zawsze rozłączne,
 * zawsze przedział [x, y] spełnia x<=y, 
 * nie ma nigdy jednocześnie przedziałów [x, y] i [y+1, z].
 * Kolejne wartości to lewe poddrzewo, przedział,
 * prawe poddrzewo, wysykość drzewa, ilość liczb całkowitych,
 * które są w tym drzewie (do funkcji below)
 *)
type t =
  | Empty
  | Node of t * (int * int) * t * int * int

let empty = Empty
  
(* Zwraca wysokość drzewa *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(* Zwraca ilość liczb całkowitych,
 * które są w tym drzewie (do funkcji below)*)
let ile_int= function
  | Node (_, _, _, _, h) -> h
  | Empty -> 0

(* Dla [a] > 0 zwraca a, wpp zwraca max_int *)
let safe_int a =
  if a < 0 then max_int else a

(* Dla a = max_int zwraca a, wpp a+1 *)
let safe_plus1 a =
  if a < a + 1 then a + 1 else a

(* Dla a = min_int zwraca a, wpp a-1 *)
let safe_minus1 a =
  if a - 1 < a then a - 1 else a
  
  
(* Tworzy drzewo z korzeniem [k] i poddrzewami [l] oraz [r]
   Zakładamy, że różnica wysokości tych poddrzew jest <=2 *)
let make l k r =
  Node (l, k, r, max (height l) (height r) + 1,
  safe_int (safe_int (ile_int l + ile_int r) +  safe_plus1 (safe_int (snd k - fst k)) ) )

(* Balansuje drzewo, tzn za pomocą rotacji częściowo wyrównuje wysokości poddrzew
   Różnica wysokości [l] i [r] musi być mniejsza równa 3, zwraca poprawne drzewo *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

(* Zwraca minimalny element w drzewie *)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(* Zwraca drzewo bez minimalnego elementu *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"
  
(* Zwraca maksymalny element *)
let rec max_elt = function
  | Node (_, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> raise Not_found

(* Zwraca drzewo bez maksymalnego elementu *)
let rec remove_max_elt = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "PSet.remove_max_elt"

(* Skleja dwa drzewa. Ich różnica wysokości jest <= 2*)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)
      
(* Sprawdza czy drzewo jest puste *)
let is_empty x = 
  x = Empty
  
(* Zwraca drzewo z nowym wierzchołkiem *)
let rec add_one x = function
  | Node (l, k, r, h, i) ->
      let c = compare x k in
      if c = 0 then Node (l, x, r, h, i)
      else if c < 0 then
        let nl = add_one x l in
        bal nl k r
      else
        let nr = add_one x r in
        bal l k nr
  | Empty -> make Empty x Empty

(* Zwraca drzewo z dodatkowym wierzchołkiem - przedziałem:
   dla przedziału (x, y) jeśli zmiejsz = -1, to dodaje przedział (x, y-1);
   jeśli zmiejsz = 1, to przedział (x+1, y), a dla zmniejsz = 0 przedział (x, y)
   Dodatkowo sprzadza czy przedział jest poprawny, tzn czy x<=y.
   *)
let rec add_pSet x zmniejsz set =
  if zmniejsz <> 0 && (fst x = snd x) then set else
  if zmniejsz =  1 then add_pSet (safe_plus1 (fst x),  snd x) 0 set else
  if zmniejsz = -1 then add_pSet (fst x, safe_minus1 (snd x)) 0 set else
  if snd x < fst x then set else
  add_one x set
  
(* Zwraca złączenie dwóch drzew*)
let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(* Usuwa z drzewa wierzchołek [x] *)
let remove_pSet x set =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = compare x k in
        if c = 0 then merge l r else
        if c < 0 then bal (loop l) k r else bal l k (loop r)
    | Empty -> Empty in
    loop set

(* Rozdziela zbiór na wierzchołki <= [x] oraz > [x] *)
let split_pSet x set =
  let rec loop x = function
      Empty ->
        (Empty, Empty)
    | Node (l, v, r, _, _) ->
        let c = compare x v in
        if c < 0 then
          let (ll, rl) = loop x l in (ll, join rl v r)
        else
          let (lr, rr) = loop x r in (join l v lr, rr)
  in
  loop x set

(** [split x s] zwraca trójkę [(l, present, r)], gdzie
    [l] to zbiór liczb mniejszych od [x];
    [r] to zbiór liczb większych  od [x];
    [present] to wartość logiczna sprawdzająca, czy [x] należy do [s]*)
let split x set =
  let (left, right) = split_pSet (x, max_int) set in
  let present = if left = Empty then false else 
    let maks = max_elt left in
      snd maks >= x && fst maks <= x
  in
  let right = if left = Empty then right else 
    let maks = max_elt left in
      if snd maks > x then add_pSet (x, snd maks) 1 right else right 
  in
  let left = if left = Empty then left else 
    let maks = max_elt left in
      if snd maks >= x  then add_pSet (fst maks , x) (-1) (remove_pSet maks left) else left
  in
  (left, present, right)

(* Zwraca największy przedział w drzewie mniejszy od [x] *)
let find_lower x set =
  let (left, right) = split_pSet (x, max_int) set in
  if left = Empty then None else Some (max_elt left)

(* Usuwa ze zbioru liczb wszystkie liczby w przedziale (x, y) *)
let rec remove (x, y) set =
  match find_lower y set with
  | None -> set
  | Some (a, b) when b < x -> set
  | Some (a, b) -> remove (x, y) (add_pSet (a, x) (-1) (add_pSet (y, b) 1 (remove_pSet (a, b) set)  ))

(* Dodaje do zbioru liczb wszystkie liczby w przedziale (x, y) *)
let rec add (x, y) set =
  let set = remove (x, y) set in
  match find_lower (safe_plus1 y) set with
  | Some (a, b) when b = safe_minus1 x -> add (a, y) set
  | Some (a, b) when a = safe_plus1  y -> add (x, b) set
  | _ -> add_pSet (x, y) 0 set

(* Zwraca wartość logiczną, czy dana liczba jest w zbiorze? *)
let mem x set =
  match find_lower x set with
  | Some (a, b) when a<=x && x<=b -> true
  | _ -> false

(* Iteruje się f do wszystkich spójnych przedziałów w zbiorze *)
let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set

(* Odpowiednik fold_left na listach, liczy
   [(f xN ... (f x2 (f x1 a))...)], gdzie x1
   ... xN to posortowane spójne przedziały danego zbioru.
*)
let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc set

(* Zwraca posortowaną listę wzsystkich spójnych przedziałów *)
let elements set = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] set
  
(** [below n s] zwraca ilość elementów [s] mniejsych lub równych [n].
    Jeśli jest ich więcej niż max_int, zwraca max_int. *)
let below n s =
  match split n s with
  | (l, false, _) -> ile_int l
  | (l, true, _) -> safe_int (1 + ile_int l)
  
(* Testy *)
let a = add_one (2, 4) (add_one (6, 10) Empty)
let a = add_one (1, 1) Empty
let a = add_one (3, 3) a
let a = add_one (5, 5) a
let a = add_one (7, 7) a
let a = add_one (9, 9) a
let a = add_one (13, 13) a
let a = add_one (11, 11) a
let a = add_one (15, 15) a
let a = add_one (17, 17) a
let a = add_one (19, 19) a
let b = find_lower 10 a
let b = add (4, 8) a
let b = add (3, 8) a
let b = add (4, 7) a
let b = add (3, 7) a

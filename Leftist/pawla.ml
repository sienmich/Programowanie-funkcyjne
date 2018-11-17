(* Autor: Paweł Pawlik 
   Reviewer: Michał Sienncki *)
(* Typ złączalnej kolejki priorytetowej. *)
type 'a queue =
  | Empty_queue
  | Node of {value : 'a; left : 'a queue; right : 'a queue; depth : int}
(* Pusta kolejka priorytetowa. *)
let empty = Empty_queue
(* Wyjątek podnoszony przez [delete_min] oraz [attach]
   gdy kolejka jest pusta. *)
exception Empty
(* Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false]. *)
let is_empty q = Empty_queue = q
(* Zwraca długość skrajnej prawej ścieżki w drzewie lub [0] jeśli drzewo
   jest puste. *)
let depth_of = function
  | Node q -> q.depth
  | _ -> 0
(* Zwraca kolejkę, której korzeniem jest [Node q], poddrzewa korzenia to
   [p1] oraz [p2]. *)

  
let asd (x : int queue) = x.value
   
let attach p1 p2 Node q =
    if depth_of p1 < depth_of p2 then 
      Node {value = q.value; left = p2; right = p1; depth = (depth_of p1) + 1}
    else
      Node {value = q.value; left = p1; right = p2; depth = (depth_of p2) + 1}
(* [join q r] zwraca złączenie kolejek [q] i [r]. *)
let rec join q r = 
  match (q, r) with
  | (Empty_queue, _) -> r
  | (_, Empty_queue) -> q
  | (Node qq, Node rr) ->
    if qq.value < rr.value then 
      attach qq.left (join qq.right r) qq
    else 
      attach rr.left (join rr.right q) rr
(* [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] 
   do kolejki [q]. *)
let add e q = 
  join (Node {value = e; left = empty; right = empty; depth = 1}) q 
(* Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
   jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
   Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min = function
  | Empty_queue -> raise Empty
  | Node q -> (q.value, join q.left q.right)
(* Testy. *)
(*  
let k = empty;;
let k = add 9 k;;
let k = add 8 k;;
let k = add 10 k;;
*)

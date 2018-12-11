type point = float * float
(** Punkt na płaszczyźnie *)

type kartka = point -> int
(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym
punkcie *)

(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty
prostokąt o bokach równoległych do osi układu współrzędnych i lewym
dolnym rogu [p1] a prawym górnym [p2]. Punkt [p1] musi więc być
nieostro na lewo i w dół od punktu [p2]. Gdy w kartkę tę wbije się 
szpilkę wewnątrz (lub na krawędziach) prostokąta, kartka zostanie
przebita 1 raz, w pozostałych przypadkach 0 razy *)
let prostokat p1 p2 =
  if fst p1 > fst p2 || snd p1 > snd p2 then invalid_arg "Origami.prostokat" else
  let pomiedzy a b x = a <= x && x <= b in
  function
  | (x, y) when pomiedzy (fst p1) (fst p2) x && pomiedzy (snd p1) (snd p2) y -> 1
  | _ -> 0

(** [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku
w punkcie [p] i promieniu [r] *)
let kolko p r =
  if r < 0. then invalid_arg "Origami.kolko" else
  function
  | (x, y) when ((fst p) -. x) *. ((fst p) -. x) +. ((snd p) -. y) *. ((snd p) -. y) <= r *. r -> 1
  | _ -> 0

let suma    a b = ((fst a) +. (fst b), (snd a) +. (snd b))

let roznica a b = ((fst a) -. (fst b), (snd a) -. (snd b))

let iloczyn_wektorowy a b =
  (fst a) *. (snd b) -. (fst b) *. (snd a)

let odbicie_wzg_prostej p1 p2 x =
  let rzut_na_prosta (x1, y1) (x2, y2) (x, y) =
    if x1 = x2 then (x1, y) else
    if y1 = y2 then (x, y1) else
    let a1 = (y2 -. y1) /. (x2 -. x1) in
    let b1 = y1 -. a1 *. x1 in
    let a = -1. /. a1 in (* współczynnk kierunkowy przy x we wzorze prostej prostopadłej do prostej zgięcia **)
    let b = y -. a *. x in
    let x_res = (b1 -. b) /. (a -. a1) in 
    let y_res = a *. x_res +. b in 
    (x_res, y_res)
  in
  let r = rzut_na_prosta p1 p2 x in
  roznica (suma r r) x
   
    

(* val zloz : point -> point -> kartka -> kartka *)
(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej
przez punkty [p1] i [p2] (muszą to być różne punkty). Papier jest
składany w ten sposób, że z prawej strony prostej (patrząc w kierunku
od [p1] do [p2]) jest przekładany na lewą. Wynikiem funkcji jest
złożona kartka. Jej przebicie po prawej stronie prostej powinno więc
zwrócić 0. Przebicie dokładnie na prostej powinno zwrócić tyle samo,
co przebicie kartki przed złożeniem. Po stronie lewej - tyle co przed
złożeniem plus przebicie rozłożonej kartki w punkcie, który nałożył
się na punkt przebicia. *)
let zloz p1 p2 kar =
  if p1 = p2 then invalid_arg "Origami.zloz" else
  function p -> match iloczyn_wektorowy (roznica p1 p) (roznica p2 p) with
  | 0. -> kar p
  | x when x < 0. -> 0  
  | _ -> (kar p) + (kar (odbicie_wzg_prostej p1 p2 p))

(* val skladaj : (point * point) list -> kartka -> kartka *)
(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)] 
czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych 
z listy *)

let skladaj lista kar =
  let zloz2 k (p1, p2) = zloz p1 p2 k in
  List.fold_left zloz2 kar lista



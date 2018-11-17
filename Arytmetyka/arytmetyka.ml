(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(*                                              *)
(* Autor rozwiązania: Michał Siennicki ms406340 *)
(*    Sprawdzający: Stanisław Strzelecki        *)
(************************************************)

(* Typ reprezentujący niedokładne wartości.       *)
(* Początek i koniec możliowego przedziału        *)
(* Jeśli początek > koniec, to chodzi o R / [x, y]*)
type wartosc = float * float

(* wszystkie liczby rzeczywiste *)
let reals = (neg_infinity, infinity)

(*dla przedziału odwrotnego zawierającego inf lub -inf
odwraca go, czyli zwraca ten sam przedział w normalnej
formie. Dla innych przedziałów jest to identyczność.*)
let normalizuj_odwrotny w = 
  if fst w =     infinity then (neg_infinity, snd w) else
  if snd w = neg_infinity then (fst w, infinity) else
  w

(* wartosc_dokladnosc x p = x +/- p% *)
(* war.pocz.: p > 0                  *)
let wartosc_dokladnosc x p   =
  if x >= 0.
  then (x *. (1. -. p /. 100.), x *. (1. +. p /. 100.))
  else (x *. (1. +. p /. 100.), x *. (1. -. p /. 100.))

(* wartosc_od_do x y = [x;y]         *)
(* war.pocz.: x <= y                 *)
let wartosc_od_do x y = 
  (x, y)

(* wartosc_dokladna x = [x;x]        *)
let wartosc_dokladna x = 
  (x, x)

(* in_wartosc w x = x \in w *)
let in_wartosc w x =
  ((fst w) <= x && x <= (snd w)) || (*normalny przedział *)
  ((snd w) < (fst w) &&       (*dla odwrotnego przedziału*)
  (x <= (snd w) || x >= (fst w))) 
  

(* min_wartosc w = najmniejsza możliwa wartość w,   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.*)
let min_wartosc w =
  let w = normalizuj_odwrotny w in
  if fst w > snd w
  then neg_infinity
  else fst w

(* max_wartosc w = największa możliwa wartość w,    *)
(* lub infinity jeśli brak górnego ograniczenia.    *)
let max_wartosc w =
  let w = normalizuj_odwrotny w in
  if fst w > snd w
  then infinity
  else snd w

(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
let rec sr_wartosc w =
  let w = normalizuj_odwrotny w in
  (min_wartosc w +. max_wartosc w) /. 2.
  
(*x, y dowolne wartości, liczy ich sumę*)
let plus x y : wartosc =
  let z = (fst x +. fst y, snd x +. snd y) in
    if ((in_wartosc x infinity) || (in_wartosc y infinity))
    && not (in_wartosc z infinity) && z = z
    (* to by oznaczało, że przedział niewłaściwy
    "przewinął się", czyli w wyniku dodawania jest właściwy*)
    then reals
    else z

(* liczy część wspólną dwóch niewłaściwych przedziałów *)
let suma_przedzialow x y =
  let wyn = (min(fst x) (fst y), max(snd x) (snd y)) in
    if fst wyn <= snd wyn
    then reals
    else wyn

(*liczy iloczyn wartości niedokładnej z dokładną*)
let razy_skalar x y =
  let bezp_mno a b = (*bezpieczne mnożenie, zawsze x*0 = 0 *)
    if a = 0. || b = 0.
    then 0.
    else a *. b
  in
    if y >= 0.
    then (bezp_mno (fst x) y, bezp_mno (snd x) y)
    else (bezp_mno (snd x) y, bezp_mno (fst x) y)

(*x, y dowolne wartości, liczy ich iloczyn*)  
let rec razy x y : wartosc =
  if x <> x || y <> y then (nan, nan) else (*czyli x lub y to nan*)
  if x = (0., 0.) || y = (0., 0.) then (0., 0.) else
  if fst x <= snd x && fst y <= snd y (* oba przedziały normalne *)
  then
    let min_z_4 a b = min (min (fst a) (snd a) ) (min (fst b) (snd b) )
    and max_z_4 a b = max (max (fst a) (snd a) ) (max (fst b) (snd b) )
    in
      ((min_z_4 (razy_skalar x (fst y)) (razy_skalar x (snd y))),
      (max_z_4 (razy_skalar x (fst y)) (razy_skalar x (snd y))))
  else
    if fst x >= snd x && fst y <= snd y (* pierwszy przedział odwrócony *)
    then
      if in_wartosc y 0.
      then 
        if fst x =     infinity then razy (neg_infinity, snd x) y else
        if snd x = neg_infinity then razy (fst x, infinity    ) y else
        reals
      else suma_przedzialow (razy_skalar x (fst y)) (razy_skalar x (snd y))
    else
      if fst x <= snd x && fst y >= snd y (* drugi przedział odwrócony *)
      then razy y x
      else          (* oba odwrócone *)
        if in_wartosc x 0. || in_wartosc y 0.
        then reals
        else suma_przedzialow (razy_skalar x (fst y)) (razy_skalar x (snd y))

(*x, y dowolne wartości, liczy ich różnicę*)    
let minus x y =
  plus x (razy y (wartosc_dokladna (-1.)))
  
(* działa dla przedziałów z końcem 0, bo (-inf, x) oznacza to samo, co
(inf,  x), bo wszystkie mniejsze od x to wszystkie poza większymi od x*)
let odwrotnosc x =
  if fst x = 0. && snd x = 0. then (nan, nan) else
  if x = reals then reals else
  (1. /. (snd x), 1. /. (fst x)) 

  
(*x, y dowolne wartości, liczy ich iloraz*)   
let podzielic x y =
  razy x (normalizuj_odwrotny (odwrotnosc y) )

(*operator porównujący dwa floaty, dopuszcza dokładość 10^-6*)
let ( =. ) (x : float) (y : float) =
  let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;

(*kilka testów*)
let _ = assert (min_wartosc (podzielic (0., 0.) (-1., 1.)) = 0.)

let _ = assert (in_wartosc (podzielic (1., 2.) (1., 2.)) 2.)

let _ = assert (in_wartosc (4., 5.) (4.5))

let _ = assert (in_wartosc (3., 5.) (3.))

let _ = assert (max_wartosc ( podzielic ( plus ( plus ( podzielic 
( wartosc_dokladnosc (0.000000) (0.000000) ) ( wartosc_od_do (-4.800000)
(0.000000) ) ) ( wartosc_od_do (-7.000000) (9.800000) ) ) ( podzielic 
( wartosc_dokladna (0.000000) ) ( wartosc_od_do (-3.200000) (-0.400000)
) ) ) ( plus ( wartosc_dokladna (0.000000) ) ( wartosc_dokladnosc 
(-8.200000) (3.800000) ) ) ) =. 0.887378936159424)  

let _ = assert  (max_wartosc ( plus ( wartosc_dokladnosc (0.000000) 
(0.000000) ) ( wartosc_od_do (-0.400000) (6.000000) )) =. 6.)

let _ = assert  (sr_wartosc ( plus ( plus ( wartosc_dokladna (2.200000) )
( wartosc_od_do (-3.200000) (8.200000) ) ) ( wartosc_od_do (-7.800000) 
(-1.200000) ) )  =. 0.199999999999999289)

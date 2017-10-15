let rec drop n l =
  if n = 0 then l
  else
    match l with
       [] -> l
    |  h :: t -> drop (n - 1) t ;;

let rec take n l =
  if n = 0 then []
  else
    match l with
      [] -> []
    | h :: t -> h :: take (n - 1) t ;;

let rec innerLength l total =
  match l with
    [] -> total
  | h :: t -> innerLength t (total + 1) ;;

let rec insert i l =
  match l with
    [] -> i :: []
  | h :: t ->
      if i < h then
        i :: insert h t
      else
        h :: insert i t ;;

let rec insertReverse i l =
  match l with
    [] -> i :: []
  | h :: t ->
    if i > h then
      i :: insertReverse h t
    else
      h :: insertReverse i t ;;

let rec sort l =
  match l with
    [] -> l
  | h :: t -> insert h (sort t) ;;

let detectCheat l = l = sort l ;;

let rec detect l =
  match l with
    [] -> true
  | [a] -> true
  | h :: m :: t ->
    if h <= m then
      detect (m :: t)
    else
      false ;;

(* Exercise: Combine two already-sorted list *)
let rec merge x y =
  match x, y with
    l, [] -> l
  | [], r -> r
  | lh :: lt, rh :: rt ->
    if lh < rh then
      lh :: merge lt (rh :: rt)
    else
      rh :: merge (lh :: lt) rt ;;

(* Exercise: merge sort *)
let rec msort l =
  match l with
    [] -> []
  | [a] -> [a]
  | _ ->
    let listlength = (innerLength l 0) / 2 in
    let left = take listlength l in
    let right = drop listlength l in
    merge (msort left) (msort right) ;;

let rec sort l =
  let rec insert i s =
    match s with
      [] -> i :: []
    | h :: t ->
        if i < h then
          i :: insert h t
        else
          h :: insert i t
  in match l with
   [] -> []
  | h :: t -> insert h (sort t) ;;

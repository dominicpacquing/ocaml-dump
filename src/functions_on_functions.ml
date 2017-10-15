(* from lists.ml *)
let rec innerLength l total =
  match l with
    [] -> total
  | h :: t -> innerLength t (total + 1) ;;

let rec take n l =
  if n = 0 then []
  else
    match l with
      [] -> []
    | h :: t -> h :: take (n - 1) t ;;

let rec drop n l =
  if n = 0 then l
  else
    match l with
       [] -> l
    |  h :: t -> drop (n - 1) t ;;

let rec map fn l =
  match l with
   [] -> []
  | h :: t -> fn h :: map fn t ;;

let to_double x = x * 2 ;;
let rec double l = map to_double l ;;

let is_even x = x mod 2 = 0 ;;
let evens l = map is_even l ;;

let evens_2 l =
  map (fun x -> x mod 2 = 0) l ;;

(* Exercise: Combine two already-sorted list with comparison function *)
let comp_for_merge x y = x < y ;;

let rec merge cmp x y =
  match x, y with
    l, [] -> l
  | [], r -> r
  | lh :: lt, rh :: rt ->
    if cmp lh rh then
      lh :: merge cmp lt (rh :: rt)
    else
      rh :: merge cmp (lh :: lt) rt ;;

(* Exercise: merge sort with comparison factor*)
let rec msort cmp l =
  match l with
    [] -> []
  | [a] -> [a]
  | _ ->
    let listlength = (innerLength l 0) / 2 in
    let left = take listlength l in
    let right = drop listlength l in
    merge cmp (msort cmp left) (msort cmp right) ;;

(* recursive *)
let rec calm l =
  match l with
   [] -> []
  | h :: t ->
    if h = '!' then
      '.' :: calm t
    else
      h :: calm t ;;

let calm_fn x = if x = '!' then '.' else x ;;

(* forces an integer to be in 1..10 range *)
let clip i =
  if i < 1 then 1
  else if i > 10 then 10 ;;

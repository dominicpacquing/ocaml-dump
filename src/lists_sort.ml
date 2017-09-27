let rec insert i l =
  match l with
    [] -> i :: []
  | h :: t ->
      if i < h then
        i :: h :: t
      else
        h :: insert i t ;;

let rec sort l =
  match l with
    [] -> l
  | h :: t -> insert h (sort t) ;;

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

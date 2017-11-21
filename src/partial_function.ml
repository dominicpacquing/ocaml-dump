(*
  signature: map fn l = ('a -> 'b) -> 'a list -> 'b list
*)
let rec map fn l =
  match l with
   [] -> []
  | h :: t -> fn h :: map fn t ;;

let rec member a l =
  match l with
   [] -> false
  | h :: t -> if h = a then true else member a t ;;

let rec mapl fn ll =
  match ll with
    [] -> []
  | h :: t -> map fn h :: mapl fn t ;;

(*
signature: map fn l = (a -> b) -> a list -> b list
(map fn): a list -> b list
map (map fn): a list list -> b list list
*)
let mapl fn ll = map (map fn) ll ;;

let member_all x lsls =
  let vals = map (member x) lsls in
    not (member false vals );;

let rdiv x y = y / x ;;

let map_div_2 l =
  map (rdiv 2) l ;;

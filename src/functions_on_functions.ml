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

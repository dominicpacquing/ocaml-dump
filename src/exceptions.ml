exception Problem ;;
exception NotPrime of int ;;

let rec take n l =
  if n < 0 then raise (Invalid_argument "take")
  else if n = 0 then []
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

let safe_divide x y =
  try x / y with
    Division_by_zero -> 0

let rec last l =
  match l with
   [] -> raise Not_found
  | [a] -> a
  | _ :: t -> last t ;;

(* Exercises*)
let rec smallest_inner l found smallest =
  match l with
    [] -> if found then smallest else raise Not_found
  | h :: t ->
    if h < smallest then
      smallest_inner t true h
    else
      smallest_inner t found smallest ;;

let smallest l = smallest_inner l false max_int ;;

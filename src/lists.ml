let rec sum n total =
  match n with
    [] -> total
  | a :: t -> sum t (a + total) ;;

let rec length l =
  match l with
    [] -> 0
  | h :: t -> 1 + length t ;;

let rec innerLength l total =
  match l with
    [] -> total
  | h :: t -> innerLength t (total + 1) ;;

let rec oddElements l =
  match l with
    [] -> []
  | [ a ] -> [ a ]
  | a :: _ :: t -> a :: oddElements t ;;

let rec append a b =
  match a with
    [] -> b
  | h :: t -> h :: append t b ;;

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

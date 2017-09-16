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

let rec evenElements l =
  match l with
   _ :: a :: t -> a :: evenElements t
  | _ -> [] ;;

let rec countTrue l total =
  match l with
    [] -> total
  | h :: t -> countTrue t (if h = true then total + 1 else total) ;;

let rec rev_accumulator l acc =
  match l with
  [] -> acc
  | a :: t -> rev_accumulator t (a :: acc) ;;

let rev l = rev_accumulator l [] ;;

let createPalindrome l =
  l @ rev l ;;

let isPalindrome l m =
  l = rev m ;;

let rec dropLast l =
  match l with
    [] -> []
  | [ a ] -> []
  | h :: t -> h :: dropLast t ;;

let rec member a l =
  match l with
   [] -> false
  | h :: t -> if h = a then true else member a t ;;

let rec make_set l =
  match l with
    [] -> []
  | h :: t ->
      if member h t = true then
        make_set t
      else
        h :: make_set t ;;

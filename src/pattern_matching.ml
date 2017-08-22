let rec factorial n =
  match n with
    1 -> 1
  | _ -> n * factorial (n - 1) ;;

let rec gcd a b =
  match b with
    0 -> a
  | _ -> gcd b (a mod b) ;;

let not x =
  match x with
    true -> false
  | _ -> true ;;

let rec power x n =
  match n with
    1 -> x
  | _ -> x * power x (n - 1) ;;

let rec sumN n =
  match n with
    1 -> n
  | o -> o + sumN (n - 1) ;;

let isLower n =
  match n with
    'a'..'z' -> true
  | _ -> false ;;

let isUpper n =
  match n with
    'A'..'Z' -> true
  | _ -> false ;;

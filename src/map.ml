let fst hm =
  match hm with
   (x, _) -> x;;

let scnd hm =
  match hm with
    (_, y) -> y ;;

let ft (x, _) = x ;;
let sn (_, y) = y ;;

let rec add k v l =
  match l with
    [] -> [(k, v)]
  | (k', t') :: t ->
    if k = k' then
      (k, v) :: t
    else
      (k', t') :: add k v t ;;

let rec remove k v l =
  match l with
    [] -> []
  | (k', v') :: t ->
    if k = k' then
      t
    else
      (k', v') :: remove k v t ;;

let rec replace k v l =
  match l with
    [] -> raise Not_found
  | (k', v') :: t ->
    if k = k' then
      (k, v) :: t
    else
      (k', v') :: replace k v t ;;

let rec innerLength l total =
  match l with
    [] -> total
  | h :: t -> innerLength t (total + 1) ;;

let rec length l = innerLength l 0 ;;

let rec innerTranspose l m =
  match l, m with
    h :: t, h' :: t' ->
      (h, h') :: innerTranspose t t'
  | _ -> [] ;;

let transpose l m =
  if (length l) = (length m) then
    innerTranspose l m
  else
    raise (Invalid_argument "arrays don't have the same size") ;;

let rec inverseTranspose listDic (kl, vl) =
  match listDic with
    [] -> (kl, vl)
  | (k, v) :: t ->
      inverseTranspose t (k :: kl, v :: vl) ;;
  

let rec factorial n =
  if n = 1 then 1 else n * factorial (n - 1);;

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b);;

let multByTen x = x * 10;;

let bothAreNonZero a b =
  a > 0 && b > 0;;

let rec sumN x =
  if x = 1 then x else x + sumN(x - 1);;

let rec power x n =
  if n = 1 then x else x * power x (n - 1);;

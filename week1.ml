(* Definitions *)
let x_power_8 =
  let x = x * x in
  let x = x * x in
  x * x;;

let sentence =
  let wordcombo = word ^ "," in
  let wordcombo2 = wordcombo ^ wordcombo in
  let wordcombo3 = wordcombo2 ^ wordcombo2 in
  wordcombo3 ^ wordcombo3 ^ word;;


(* Functions *)
let multiple_of n d =
  if n mod d = 0 then true else false;;

let integer_square_root n =
  int_of_float(sqrt (float_of_int n));;

let last_character str =
  String.get str (String.length str - 1)

let string_of_bool truth =
  if truth = true then "true" else "false";;


(* Recursion *)
let rec gcd n m =
  if n = 0 then m else if m = 0 then n else gcd m (n mod m);;

let rec multiple_upto n r =
  if r < 2 then false else if n mod r = 0 then true else multiple_upto n (r-1);;

let is_prime n =
  if n == 1 then false else if n == 2 then true else if multiple_upto n (int_of_float (sqrt (float_of_int n))) then false else true;;

let is_prime n =
  let int_sqrt = int_of_float (sqrt (float_of_int n)) in
  let multiple_upto n r = if r < 2 then false else if n mod r = 0 then true else multiple_upto n (r-1) in
  if n < 2 then false else if multiple_upto n int_sqrt then false else true;;

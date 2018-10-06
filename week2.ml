(* Tuples *)
let exchange k =
  let k_str = (string_of_int k) in
  int_of_string (String.make 1 k_str.[1] ^ String.make 1 k_str.[0]);;
let exchange k = k mod 10 * 10 + k / 10;;

let is_valid_answer (grand_father_age, grand_son_age) =
  if (grand_son_age * 4 = grand_father_age) &&
     (exchange grand_father_age * 3 = exchange grand_son_age)
  then true else false;;

let find (max_grand_father_age, min_grand_son_age) =
  let rec aux gs max_gf =
    let gf = gs * 4 in
      if gs > max_gf then (-1, -1) else
      if is_valid_answer(gf, gs) then (gf, gs) else aux (gs + 1) max_gf
  in aux min_grand_son_age max_grand_father_age;;


(* Records -- Point and Vectors *)
type point  = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }

let move (p: point) (dp: dpoint) =
  { x = p.x +. dp.dx; y = p.y +. dp.dy; z = p.z +. dp.dz };;

let next obj =
  let p = obj.position in
  let dp = obj.velocity in
  {position = move p dp; velocity = dp};;

let will_collide_soon p1 p2 =
  let next_p1 = (next p1).position in
  let next_p2 = (next p2).position in
  if ((next_p1.x -. next_p2.x) ** 2. +. (next_p1.y -. next_p2.y) ** 2. +. (next_p1.z -. next_p2.z) ** 2.) ** 0.5 < (1. +. 1.)
    then true else false;;

(* Records -- Time on Planet Shadokus *)
type date =
  { year : int; month : int; day : int;
    hour : int; minute : int }

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 }

let wellformed { year; month; day; hour; minute } =
  year >= 1 && month >= 1 && month <= 5 && day >= 1 && day <= 4 &&
    hour >= 0 && hour <= 2 && minute >= 0 && minute <= 1;;

let next { year; month; day; hour; minute } =
  let date_to_try = { year=year; month=month; day=day; hour=hour; minute=minute+1} in
  if wellformed date_to_try = true then date_to_try else
    let date_to_try = { year=year; month=month; day=day; hour=hour+1; minute=0} in
    if wellformed date_to_try = true then date_to_try else
      let date_to_try = { year=year; month=month; day=day+1; hour=0; minute=0} in
      if wellformed date_to_try = true then date_to_try else
        let date_to_try = { year=year; month=month+1; day=1; hour=0; minute=0} in
        if wellformed date_to_try = true
          then date_to_try else { year=year+1; month=1; day=1; hour=0; minute=0};;

let rec add_min (date, mins_to_add) =
  if mins_to_add = 0 then date else add_min ( next date, mins_to_add - 1 );;

let of_int minutes =
  add_min (the_origin_of_time, minutes);;


(* Arrays *)
let rec curr_min obj =
  let arr, length = obj in
    if length == 1
    then arr.(0)
    else
      let min_of_array = curr_min (Array.sub arr 1 (length - 1), length - 1) in
        if arr.(0) < min_of_array then arr.(0) else min_of_array;;

let min a =
  curr_min (a, Array.length a);;

let rec curr_min_idx obj =
  let arr, length = obj in
    if length == 1
    then arr.(0), length
    else
      let curr_min, idx = curr_min_idx (Array.sub arr 1 (length - 1), length - 1) in
        if arr.(0) < curr_min then arr.(0), length else curr_min, idx;;

let min_index a =
  let length = Array.length a in
  let min, idx = curr_min_idx (a, length) in length - idx;;

let it_scales = "no" ;;

let rec is_sorted a =
  let length = Array.length a in
    if length <= 1
      then true
      else if a.(0) >= a.(1) then false else is_sorted (Array.sub a 1 (length - 1));;

let rec binary_search arr word_to_find lo hi =
  let mid = (hi + lo) / 2 in
    if hi < lo then -1 else
    let word = arr.(mid) in
      if word_to_find = word then mid else
      if word_to_find < word
      then
        binary_search arr word_to_find lo (mid - 1)
      else
        binary_search arr word_to_find (mid + 1) hi;;

let find dict word_to_find =
  let length = Array.length dict in
  binary_search dict word_to_find 0 (length - 1);;

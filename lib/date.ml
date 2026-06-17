open Core

type t = int [@@deriving sexp, compare, equal, hash]

let is_leap y = (y mod 4 = 0 && y mod 100 <> 0) || (y mod 400 = 0)

let days_in_month y m =
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap y then 29 else 28
  | _ -> failwith "invalid month"

(* Count leap years in [1970, y-1] inclusive *)
let leaps_before y =
  (y / 4) - (y / 100) + (y / 400)

(* Days from 1970-01-01 to year y, jan 1 *)
let days_to_year y =
  let y' = y - 1970 in
  let leaps = leaps_before (y - 1) - leaps_before 1969 in
  y' * 365 + leaps

let of_ymd year month day =
  let d = ref (days_to_year year) in
  for m = 1 to month - 1 do
    d := !d + days_in_month year m
  done;
  !d + day - 1

let to_ymd n =
  let approx_year = 1970 + n / 366 in
  let year = ref approx_year in
  (* Advance until days_to_year (year+1) > n *)
  while days_to_year (!year + 1) <= n do incr year done;
  (* Retreat if we went too far *)
  while days_to_year !year > n do decr year done;
  let remaining = ref (n - days_to_year !year) in
  let month = ref 1 in
  while !month < 12 && !remaining >= days_in_month !year !month do
    remaining := !remaining - days_in_month !year !month;
    incr month
  done;
  (!year, !month, !remaining + 1)

let add_days t d = t + d
let diff_days t1 t2 = t1 - t2

let today () =
  let t = Core_unix.gettimeofday () in
  Int.of_float (t /. 86400.0)

let to_string t =
  let y, m, d = to_ymd t in
  Printf.sprintf "%04d-%02d-%02d" y m d

let of_string s =
  match String.split s ~on:'-' with
  | [y; m; d] -> of_ymd (Int.of_string y) (Int.of_string m) (Int.of_string d)
  | _ -> failwith ("invalid date: " ^ s)

type day_count =
  | Act365
  | Act360
  | Thirty360
[@@deriving sexp]

let year_frac dc start_ end_ =
  match dc with
  | Act365 -> Float.of_int (diff_days end_ start_) /. 365.0
  | Act360 -> Float.of_int (diff_days end_ start_) /. 360.0
  | Thirty360 ->
    let y1, m1, d1 = to_ymd start_ in
    let y2, m2, d2 = to_ymd end_ in
    let d1' = min d1 30 in
    let d2' = if d1 >= 30 then min d2 30 else d2 in
    let n = 360 * (y2 - y1) + 30 * (m2 - m1) + (d2' - d1') in
    Float.of_int n /. 360.0

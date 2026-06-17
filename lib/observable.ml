module Skew_date = Date
module Skew_currency = Currency
open Core

type _ t =
  | Const    : 'a -> 'a t
  | Lift1    : ('a -> 'b) * 'a t -> 'b t
  | Lift2    : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t
  | Date     : Skew_date.t t
  | Spot     : string -> float t
  | Horizon  : Skew_date.t -> float t
  | Rate     : Skew_currency.t -> float t
  | Greater  : float t * float t -> bool t
  | Equal    : float t * float t -> bool t
  | If       : bool t * 'a t * 'a t -> 'a t

let konst x = Const x
let spot s = Spot s
let date = Date

(* Float observable arithmetic operators *)
let ( +. ) a b = Lift2 (( +. ), a, b)
let ( -. ) a b = Lift2 (( -. ), a, b)
let ( *. ) a b = Lift2 (( *. ), a, b)
let ( /. ) a b = Lift2 (( /. ), a, b)
let max2 a b = Lift2 (Float.max, a, b)
let min2 a b = Lift2 (Float.min, a, b)
let ( >. ) a b = Greater (a, b)
let ( =. ) a b = Equal (a, b)
let iff c t f = If (c, t, f)

type env = {
  spots    : (string, float) Hashtbl.t;
  rates    : (Skew_currency.t, float) Hashtbl.t;
  cur_date : Skew_date.t;
}

let make_env ~spots ~rates ~cur_date = { spots; rates; cur_date }

let rec eval : type a. env -> a t -> a = fun env obs ->
  match obs with
  | Const x -> x
  | Lift1 (f, a) -> f (eval env a)
  | Lift2 (f, a, b) -> f (eval env a) (eval env b)
  | Date -> env.cur_date
  | Spot s ->
    (match Hashtbl.find env.spots s with
     | Some v -> v
     | None -> failwith (Printf.sprintf "Unknown spot: %s" s))
  | Horizon _ -> failwith "Horizon observable not yet implemented"
  | Rate ccy ->
    (match Hashtbl.find env.rates ccy with
     | Some r -> r
     | None -> 0.0)
  | Greater (a, b) -> Float.(eval env a > eval env b)
  | Equal (a, b) -> Float.(eval env a = eval env b)
  | If (c, t, f) -> if eval env c then eval env t else eval env f

(* to_string: for Const, try to display as float; fall back to "<val>" *)
let rec to_string : type a. a t -> string = fun obs ->
  match obs with
  | Const _ ->
    (* We can't know 'a at runtime, so use Obj.magic cautiously for display only *)
    (try
       let f : float = Obj.magic obs in
       ignore f;
       (* Actually, Const x where x : float — try to display *)
       let v : float = Obj.magic (match obs with Const x -> x | _ -> assert false) in
       Printf.sprintf "%.6g" v
     with _ -> "<const>")
  | Lift1 (_, a) -> Printf.sprintf "f(%s)" (to_string a)
  | Lift2 (_, a, b) -> Printf.sprintf "(%s op %s)" (to_string a) (to_string b)
  | Date -> "Date"
  | Spot s -> Printf.sprintf "Spot(%s)" s
  | Horizon d -> Printf.sprintf "Horizon(%s)" (Skew_date.to_string d)
  | Rate ccy -> Printf.sprintf "Rate(%s)" (Skew_currency.to_string ccy)
  | Greater (a, b) -> Printf.sprintf "(%s > %s)" (to_string a) (to_string b)
  | Equal (a, b) -> Printf.sprintf "(%s = %s)" (to_string a) (to_string b)
  | If (c, t, f) ->
    Printf.sprintf "if %s then %s else %s" (to_string c) (to_string t) (to_string f)

(* Specialized to_string for float observables *)
let float_to_string (obs : float t) : string = to_string obs

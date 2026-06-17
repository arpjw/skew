module Skew_date = Date
module Skew_currency = Currency
open Core

type rng = { mutable state: Random.State.t }

let create_rng seed = { state = Random.State.make [| seed |] }
let default_rng () = { state = Random.State.make_self_init () }

let standard_normal rng =
  (* Box-Muller transform *)
  let u1 = ref (Random.State.float rng.state 1.0) in
  while Float.(!u1 = 0.0) do
    u1 := Random.State.float rng.state 1.0
  done;
  let u2 = Random.State.float rng.state 1.0 in
  let r = Float.sqrt (-2.0 *. Float.log !u1) in
  let theta = 2.0 *. Float.pi *. u2 in
  r *. Float.cos theta

type path = {
  underlying : string;
  dates      : Skew_date.t array;
  prices     : float array;
}

let simulate_gbm ~rng ~underlying ~spot ~vol ~rate ~start_date ~dates =
  let n = Array.length dates in
  let prices = Array.create ~len:n spot in
  if n > 0 then begin
    (* Simulate from start_date to dates.(0) *)
    let prev_date  = ref start_date in
    let prev_price = ref spot in
    for i = 0 to n - 1 do
      let dt = Skew_date.year_frac Skew_date.Act365 !prev_date dates.(i) in
      let dt = Float.max dt 1e-10 in
      let z = standard_normal rng in
      let s = !prev_price *.
              Float.exp ((rate -. 0.5 *. vol *. vol) *. dt
                         +. vol *. Float.sqrt dt *. z) in
      prices.(i) <- s;
      prev_date  := dates.(i);
      prev_price := s
    done
  end;
  { underlying; dates; prices }

type scenario = {
  paths      : (string, path) Hashtbl.t;
  eval_dates : Skew_date.t array;
}

let simulate_scenario ~rng ~market ~underlyings ~eval_dates =
  let paths = Hashtbl.create (module String) in
  List.iter underlyings ~f:(fun u ->
    let spot  = Option.value (Market.get_spot market u) ~default:100.0 in
    let vol   = Market.get_vol market u ~strike:spot ~expiry:1.0 in
    let rate  = Market.get_rate market Skew_currency.USD in
    let path  = simulate_gbm ~rng ~underlying:u ~spot ~vol ~rate
                  ~start_date:market.Market.valuation_date
                  ~dates:eval_dates in
    Hashtbl.set paths ~key:u ~data:path);
  { paths; eval_dates }

(* Find price at a date: find the nearest eval date *)
let price_at_date (p : path) (d : Skew_date.t) : float =
  let n = Array.length p.dates in
  if n = 0 then 0.0
  else begin
    let best     = ref 0 in
    let best_dist = ref (abs (Skew_date.diff_days p.dates.(0) d)) in
    for i = 1 to n - 1 do
      let dist = abs (Skew_date.diff_days p.dates.(i) d) in
      if dist < !best_dist then begin
        best      := i;
        best_dist := dist
      end
    done;
    p.prices.(!best)
  end

let eval_obs_on_path ~scenario ~market ~date obs =
  let env = Market.to_obs_env market date in
  (* Override spots with path prices at the given date *)
  Hashtbl.iteri scenario.paths ~f:(fun ~key:u ~data:p ->
    Hashtbl.set env.Observable.spots ~key:u ~data:(price_at_date p date));
  Observable.eval env obs

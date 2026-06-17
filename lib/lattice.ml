module Skew_date = Date
open Core

type node = {
  price : float;
  mutable value : float;
}

type lattice = {
  n_steps : int;
  dt      : float;
  u       : float;
  d       : float;
  pu      : float;
  pd      : float;
  nodes   : node array array;
}

let build ~spot ~vol ~rate ~t_years ~n_steps =
  let dt = t_years /. Float.of_int n_steps in
  let u  = Float.exp (vol *. Float.sqrt dt) in
  let d  = 1.0 /. u in
  let pu = (Float.exp (rate *. dt) -. d) /. (u -. d) in
  let pd = 1.0 -. pu in
  let nodes = Array.init (n_steps + 1) ~f:(fun step ->
    Array.init (step + 1) ~f:(fun j ->
      let price = spot *. (u ** Float.of_int j) *. (d ** Float.of_int (step - j)) in
      { price; value = 0.0 }))
  in
  { n_steps; dt; u; d; pu; pd; nodes }

let price_european ~spot ~vol ~rate ~t_years ~n_steps ~payoff =
  let lat = build ~spot ~vol ~rate ~t_years ~n_steps in
  let df  = Float.exp (-. rate *. lat.dt) in
  (* Terminal payoffs *)
  let terminal = lat.nodes.(n_steps) in
  Array.iter terminal ~f:(fun node ->
    node.value <- Float.max 0.0 (payoff node.price));
  (* Backward induction *)
  for step = n_steps - 1 downto 0 do
    let cur  = lat.nodes.(step) in
    let next = lat.nodes.(step + 1) in
    Array.iteri cur ~f:(fun j node ->
      node.value <- df *. (lat.pu *. next.(j+1).value +. lat.pd *. next.(j).value))
  done;
  lat.nodes.(0).(0).value

let price_american ~spot ~vol ~rate ~t_years ~n_steps ~payoff =
  let lat = build ~spot ~vol ~rate ~t_years ~n_steps in
  let df  = Float.exp (-. rate *. lat.dt) in
  let terminal = lat.nodes.(n_steps) in
  Array.iter terminal ~f:(fun node ->
    node.value <- Float.max 0.0 (payoff node.price));
  for step = n_steps - 1 downto 0 do
    let cur  = lat.nodes.(step) in
    let next = lat.nodes.(step + 1) in
    Array.iteri cur ~f:(fun j node ->
      let continuation =
        df *. (lat.pu *. next.(j+1).value +. lat.pd *. next.(j).value) in
      let exercise = Float.max 0.0 (payoff node.price) in
      node.value <- Float.max continuation exercise)
  done;
  lat.nodes.(0).(0).value

let price_european_call ~spot ~vol ~rate ~t_years ~n_steps ~strike =
  price_european ~spot ~vol ~rate ~t_years ~n_steps
    ~payoff:(fun s -> s -. strike)

let price_european_put ~spot ~vol ~rate ~t_years ~n_steps ~strike =
  price_european ~spot ~vol ~rate ~t_years ~n_steps
    ~payoff:(fun s -> strike -. s)

let price_american_call ~spot ~vol ~rate ~t_years ~n_steps ~strike =
  price_american ~spot ~vol ~rate ~t_years ~n_steps
    ~payoff:(fun s -> s -. strike)

let price_american_put ~spot ~vol ~rate ~t_years ~n_steps ~strike =
  price_american ~spot ~vol ~rate ~t_years ~n_steps
    ~payoff:(fun s -> strike -. s)

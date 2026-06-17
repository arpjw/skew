module Skew_date = Date
open Core

let test_path_length () =
  let rng   = Path.create_rng 42 in
  let start = Skew_date.of_ymd 2025 1 1 in
  let dates = Array.init 10 ~f:(fun i -> Skew_date.add_days start i) in
  let p = Path.simulate_gbm ~rng ~underlying:"X"
            ~spot:100.0 ~vol:0.2 ~rate:0.05
            ~start_date:start ~dates in
  Alcotest.(check int) "path length" 10 (Array.length p.Path.prices)

let test_zero_vol () =
  let rng   = Path.create_rng 42 in
  let start = Skew_date.of_ymd 2025 1 1 in
  let dates = Array.init 5 ~f:(fun i -> Skew_date.add_days start i) in
  let p = Path.simulate_gbm ~rng ~underlying:"X"
            ~spot:100.0 ~vol:0.0 ~rate:0.0
            ~start_date:start ~dates in
  Array.iter p.Path.prices ~f:(fun price ->
    Alcotest.(check (float 0.01)) "zero vol = constant" 100.0 price)

let test_gbm_mean () =
  (* Mean of GBM at T=1 should be spot * exp(rate * T) *)
  let n     = 5_000 in
  let spot  = 100.0 in
  let rate  = 0.05 in
  let t     = 1.0 in
  let start = Skew_date.of_ymd 2025 1 1 in
  let end_  = Skew_date.add_days start (Int.of_float (t *. 365.0)) in
  let dates = [| end_ |] in
  let total = ref 0.0 in
  for seed = 0 to n - 1 do
    let rng = Path.create_rng seed in
    let p   = Path.simulate_gbm ~rng ~underlying:"X"
                ~spot ~vol:0.2 ~rate
                ~start_date:start ~dates in
    total := !total +. p.Path.prices.(0)
  done;
  let mean     = !total /. Float.of_int n in
  let expected = spot *. Float.exp (rate *. t) in
  (* Allow 3% tolerance given finite sample *)
  Alcotest.(check (float 3.5)) "gbm mean" expected mean

let tests =
  [ "path_length", `Quick, test_path_length
  ; "zero_vol",    `Quick, test_zero_vol
  ; "gbm_mean",    `Slow,  test_gbm_mean
  ]

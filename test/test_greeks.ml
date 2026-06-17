module Skew_date = Date
module Skew_currency = Currency
open Core

let market () =
  let m = Market.create ~valuation_date:(Skew_date.of_ymd 2025 1 1) in
  Market.set_spot m "AAPL" 100.0;
  Market.set_flat_vol m "AAPL" 0.2;
  Market.set_rate m Skew_currency.USD 0.05;
  m

let expiry = Skew_date.of_ymd 2026 1 1

let test_delta_zero () =
  let d = Greeks.delta ~market:(market ()) ~contract:Contract.Zero
            ~underlying:"AAPL" ~n_paths:1_000 in
  Alcotest.(check (float 0.01)) "delta zero = 0" 0.0 d

let test_delta_call () =
  (* BS delta for ATM call S=100, K=100, T=1, r=0.05, sigma=0.2 is ~0.636 *)
  let c = Contract.european_call "AAPL" Skew_currency.USD 100.0 expiry in
  let d = Greeks.delta ~market:(market ()) ~contract:c
            ~underlying:"AAPL" ~n_paths:50_000 in
  Alcotest.(check (float 0.05)) "delta call ~0.636" 0.636 d

let test_delta_give () =
  (* delta of Give(c) = -delta(c) *)
  let c   = Contract.european_call "AAPL" Skew_currency.USD 100.0 expiry in
  let d   = Greeks.delta ~market:(market ()) ~contract:c
              ~underlying:"AAPL" ~n_paths:10_000 in
  let dg  = Greeks.delta ~market:(market ())
              ~contract:(Contract.Give c)
              ~underlying:"AAPL" ~n_paths:10_000 in
  Alcotest.(check (float 0.05)) "delta(give c) = -delta(c)" (-. d) dg

let tests =
  [ "delta_zero", `Quick, test_delta_zero
  ; "delta_call", `Slow,  test_delta_call
  ; "delta_give", `Slow,  test_delta_give
  ]

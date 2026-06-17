module Skew_date = Date
module Skew_currency = Currency
open Core

let make_env () : Observable.env =
  let spots = Hashtbl.of_alist_exn (module String) [("AAPL", 155.0)] in
  let rates = Hashtbl.create (module Skew_currency) in
  { Observable.spots; rates; cur_date = Skew_date.of_ymd 2025 1 1 }

let test_const () =
  let env = make_env () in
  Alcotest.(check (float 1e-9)) "const 42"
    42.0 (Observable.eval env (Observable.Const 42.0))

let test_spot () =
  let env = make_env () in
  Alcotest.(check (float 1e-9)) "spot AAPL"
    155.0 (Observable.eval env (Observable.Spot "AAPL"))

let test_lift2 () =
  let env = make_env () in
  let obs = Observable.Lift2 (( +. ), Observable.Const 1.0, Observable.Const 2.0) in
  Alcotest.(check (float 1e-9)) "1+2=3" 3.0 (Observable.eval env obs)

let test_greater_true () =
  let env = make_env () in
  Alcotest.(check bool) "5>3"
    true
    (Observable.eval env (Observable.Greater (Observable.Const 5.0, Observable.Const 3.0)))

let test_greater_false () =
  let env = make_env () in
  Alcotest.(check bool) "1>3 false"
    false
    (Observable.eval env (Observable.Greater (Observable.Const 1.0, Observable.Const 3.0)))

let test_if_true () =
  let env = make_env () in
  let obs = Observable.(If (Greater (Const 5.0, Const 3.0), Const 1.0, Const 0.0)) in
  Alcotest.(check (float 1e-9)) "if true" 1.0 (Observable.eval env obs)

let test_if_false () =
  let env = make_env () in
  let obs = Observable.(If (Greater (Const 1.0, Const 3.0), Const 1.0, Const 0.0)) in
  Alcotest.(check (float 1e-9)) "if false" 0.0 (Observable.eval env obs)

let test_to_string () =
  Alcotest.(check string) "Spot(AAPL)"
    "Spot(AAPL)" (Observable.to_string (Observable.Spot "AAPL"))

let tests =
  [ "const",        `Quick, test_const
  ; "spot",         `Quick, test_spot
  ; "lift2",        `Quick, test_lift2
  ; "greater_true", `Quick, test_greater_true
  ; "greater_false",`Quick, test_greater_false
  ; "if_true",      `Quick, test_if_true
  ; "if_false",     `Quick, test_if_false
  ; "to_string",    `Quick, test_to_string
  ]

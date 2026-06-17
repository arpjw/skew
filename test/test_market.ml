module Skew_date = Date

let test_spot_round_trip () =
  let m = Market.create ~valuation_date:(Skew_date.of_ymd 2025 1 1) in
  Market.set_spot m "AAPL" 155.0;
  Alcotest.(check (option (float 1e-9))) "spot round-trip"
    (Some 155.0) (Market.get_spot m "AAPL")

let test_spot_missing () =
  let m = Market.create ~valuation_date:(Skew_date.of_ymd 2025 1 1) in
  Alcotest.(check (option (float 1e-9))) "missing spot"
    None (Market.get_spot m "AAPL")

let test_flat_vol () =
  let m = Market.create ~valuation_date:(Skew_date.of_ymd 2025 1 1) in
  Market.set_flat_vol m "AAPL" 0.25;
  let v = Market.get_vol m "AAPL" ~strike:100.0 ~expiry:1.0 in
  Alcotest.(check (float 0.01)) "flat vol" 0.25 v

let test_flat_vol_any_strike () =
  let m = Market.create ~valuation_date:(Skew_date.of_ymd 2025 1 1) in
  Market.set_flat_vol m "AAPL" 0.30;
  let v1 = Market.get_vol m "AAPL" ~strike:50.0  ~expiry:0.5 in
  let v2 = Market.get_vol m "AAPL" ~strike:200.0 ~expiry:5.0 in
  Alcotest.(check (float 0.01)) "flat vol any strike 1" 0.30 v1;
  Alcotest.(check (float 0.01)) "flat vol any strike 2" 0.30 v2

let test_obs_env () =
  let m = Market.create ~valuation_date:(Skew_date.of_ymd 2025 1 1) in
  Market.set_spot m "AAPL" 155.0;
  let env = Market.to_obs_env m (Skew_date.of_ymd 2025 1 1) in
  let v = Observable.eval env (Observable.Spot "AAPL") in
  Alcotest.(check (float 1e-9)) "obs env spot" 155.0 v

let tests =
  [ "spot_round_trip",    `Quick, test_spot_round_trip
  ; "spot_missing",       `Quick, test_spot_missing
  ; "flat_vol",           `Quick, test_flat_vol
  ; "flat_vol_any_strike",`Quick, test_flat_vol_any_strike
  ; "obs_env",            `Quick, test_obs_env
  ]

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

(* Black-Scholes analytical call price *)
let bs_call ~s ~k ~r ~t ~sigma =
  let d1 =
    (Float.log (s /. k) +. (r +. 0.5 *. sigma *. sigma) *. t)
    /. (sigma *. Float.sqrt t)
  in
  let d2 = d1 -. sigma *. Float.sqrt t in
  let n x = 0.5 *. Stdlib.Float.erfc (-. x /. Float.sqrt 2.0) in
  s *. n d1 -. k *. Float.exp (-. r *. t) *. n d2

let bs_put ~s ~k ~r ~t ~sigma =
  let d1 =
    (Float.log (s /. k) +. (r +. 0.5 *. sigma *. sigma) *. t)
    /. (sigma *. Float.sqrt t)
  in
  let d2 = d1 -. sigma *. Float.sqrt t in
  let n x = 0.5 *. Stdlib.Float.erfc (-. x /. Float.sqrt 2.0) in
  k *. Float.exp (-. r *. t) *. (1.0 -. n d2) -. s *. (1.0 -. n d1)

let test_price_zero () =
  let config = Pricer.MonteCarlo.default_config in
  let p = Pricer.MonteCarlo.price ~config ~market:(market ()) ~contract:Contract.Zero in
  Alcotest.(check (float 0.01)) "price zero = 0" 0.0 p

let test_lattice_zero () =
  let config = Pricer.LatticePricer.default_config in
  let p = Pricer.LatticePricer.price ~config ~market:(market ()) ~contract:Contract.Zero in
  Alcotest.(check (float 1e-9)) "lattice zero = 0" 0.0 p

let test_european_call_mc () =
  let config = Pricer.MonteCarlo.{ default_config with n_paths = 50_000 } in
  let c = Contract.european_call "AAPL" Skew_currency.USD 100.0 expiry in
  let p = Pricer.MonteCarlo.price ~config ~market:(market ()) ~contract:c in
  let bs = bs_call ~s:100.0 ~k:100.0 ~r:0.05 ~t:1.0 ~sigma:0.2 in
  Alcotest.(check (float 0.5)) "mc call vs BS" bs p

let test_lattice_call () =
  let config = Pricer.LatticePricer.default_config in
  let c = Contract.european_call "AAPL" Skew_currency.USD 100.0 expiry in
  let p = Pricer.LatticePricer.price ~config ~market:(market ()) ~contract:c in
  let bs = bs_call ~s:100.0 ~k:100.0 ~r:0.05 ~t:1.0 ~sigma:0.2 in
  Alcotest.(check (float 0.05)) "lattice call vs BS" bs p

let test_lattice_put () =
  let config = Pricer.LatticePricer.default_config in
  let c = Contract.european_put "AAPL" Skew_currency.USD 100.0 expiry in
  let p = Pricer.LatticePricer.price ~config ~market:(market ()) ~contract:c in
  let bs = bs_put ~s:100.0 ~k:100.0 ~r:0.05 ~t:1.0 ~sigma:0.2 in
  Alcotest.(check (float 0.05)) "lattice put vs BS" bs p

let test_put_call_parity_mc () =
  let config = Pricer.MonteCarlo.{ default_config with n_paths = 50_000 } in
  let call   = Contract.european_call "AAPL" Skew_currency.USD 100.0 expiry in
  let put    = Contract.european_put  "AAPL" Skew_currency.USD 100.0 expiry in
  let pc = Pricer.MonteCarlo.price ~config ~market:(market ()) ~contract:call in
  let pp = Pricer.MonteCarlo.price ~config ~market:(market ()) ~contract:put  in
  let parity = 100.0 -. 100.0 *. Float.exp (-. 0.05 *. 1.0) in
  Alcotest.(check (float 0.30)) "put-call parity MC" parity (pc -. pp)

let test_put_call_parity_lattice () =
  let config  = Pricer.LatticePricer.default_config in
  let call    = Contract.european_call "AAPL" Skew_currency.USD 100.0 expiry in
  let put     = Contract.european_put  "AAPL" Skew_currency.USD 100.0 expiry in
  let pc = Pricer.LatticePricer.price ~config ~market:(market ()) ~contract:call in
  let pp = Pricer.LatticePricer.price ~config ~market:(market ()) ~contract:put  in
  let parity  = 100.0 -. 100.0 *. Float.exp (-. 0.05 *. 1.0) in
  Alcotest.(check (float 0.01)) "put-call parity lattice" parity (pc -. pp)

let test_give_reverses_sign () =
  let config = Pricer.MonteCarlo.{ default_config with n_paths = 1_000 } in
  let c  = Contract.One Skew_currency.USD in
  let p  = Pricer.MonteCarlo.price ~config ~market:(market ()) ~contract:c in
  let pg = Pricer.MonteCarlo.price ~config ~market:(market ())
             ~contract:(Contract.Give c) in
  Alcotest.(check (float 0.01)) "give reverses sign" (-. p) pg

let test_american_put_ge_european () =
  let lat_cfg = Pricer.LatticePricer.default_config in
  let am  = Contract.american_put  "AAPL" Skew_currency.USD 100.0 expiry in
  let eu  = Contract.european_put  "AAPL" Skew_currency.USD 100.0 expiry in
  let pam = Pricer.LatticePricer.price ~config:lat_cfg ~market:(market ()) ~contract:am in
  let peu = Pricer.LatticePricer.price ~config:lat_cfg ~market:(market ()) ~contract:eu in
  Alcotest.(check bool) "american put >= european put" true Float.(pam >= peu -. 0.001)

let tests =
  [ "price_zero",              `Quick, test_price_zero
  ; "lattice_zero",            `Quick, test_lattice_zero
  ; "european_call_mc",        `Slow,  test_european_call_mc
  ; "lattice_call",            `Quick, test_lattice_call
  ; "lattice_put",             `Quick, test_lattice_put
  ; "put_call_parity_mc",      `Slow,  test_put_call_parity_mc
  ; "put_call_parity_lattice", `Quick, test_put_call_parity_lattice
  ; "give_reverses_sign",      `Quick, test_give_reverses_sign
  ; "american_put_ge_european",`Quick, test_american_put_ge_european
  ]

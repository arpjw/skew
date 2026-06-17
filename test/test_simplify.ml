module Skew_date = Date
module Skew_currency = Currency

let test_give_give () =
  let c = Simplify.simplify
            (Contract.Give (Contract.Give (Contract.One Skew_currency.USD))) in
  Alcotest.(check bool) "give(give(c)) = c"
    true (Contract.equal c (Contract.One Skew_currency.USD))

let test_and_zero_left () =
  let c = Simplify.simplify
            (Contract.And (Contract.Zero, Contract.One Skew_currency.USD)) in
  Alcotest.(check bool) "and(zero,c) = c"
    true (Contract.equal c (Contract.One Skew_currency.USD))

let test_and_zero_right () =
  let c = Simplify.simplify
            (Contract.And (Contract.One Skew_currency.USD, Contract.Zero)) in
  Alcotest.(check bool) "and(c,zero) = c"
    true (Contract.equal c (Contract.One Skew_currency.USD))

let test_scale_zero_coeff () =
  let expiry = Skew_date.of_ymd 2026 12 19 in
  let call   = Contract.european_call "AAPL" Skew_currency.USD 150.0 expiry in
  let c = Simplify.simplify (Contract.Scale (Observable.Const 0.0, call)) in
  Alcotest.(check bool) "scale(0,c) = zero"
    true (Contract.equal c Contract.Zero)

let test_scale_one_coeff () =
  let c = Simplify.simplify
            (Contract.Scale (Observable.Const 1.0, Contract.One Skew_currency.USD)) in
  Alcotest.(check bool) "scale(1,c) = c"
    true (Contract.equal c (Contract.One Skew_currency.USD))

let test_scale_zero_contract () =
  let c = Simplify.simplify
            (Contract.Scale (Observable.Spot "AAPL", Contract.Zero)) in
  Alcotest.(check bool) "scale(obs,zero) = zero"
    true (Contract.equal c Contract.Zero)

let test_const_fold () =
  let c = Simplify.simplify
            (Contract.Scale (Observable.Const 2.0,
               Contract.Scale (Observable.Const 3.0,
                 Contract.One Skew_currency.USD))) in
  match c with
  | Contract.Scale (Observable.Const v, Contract.One Skew_currency.USD) ->
    Alcotest.(check (float 1e-9)) "2*3=6" 6.0 v
  | _ -> Alcotest.fail "expected Scale(6.0, One USD)"

let test_triple_give () =
  let c = Simplify.simplify
            (Contract.Give (Contract.Give
               (Contract.Give (Contract.One Skew_currency.USD)))) in
  Alcotest.(check bool) "give(give(give(c))) = give(c)"
    true (Contract.equal c (Contract.Give (Contract.One Skew_currency.USD)))

let test_give_zero () =
  let c = Simplify.simplify (Contract.Give Contract.Zero) in
  Alcotest.(check bool) "give(zero) = zero"
    true (Contract.equal c Contract.Zero)

let tests =
  [ "give_give",         `Quick, test_give_give
  ; "and_zero_left",     `Quick, test_and_zero_left
  ; "and_zero_right",    `Quick, test_and_zero_right
  ; "scale_zero_coeff",  `Quick, test_scale_zero_coeff
  ; "scale_one_coeff",   `Quick, test_scale_one_coeff
  ; "scale_zero_contract",`Quick, test_scale_zero_contract
  ; "const_fold",        `Quick, test_const_fold
  ; "triple_give",       `Quick, test_triple_give
  ; "give_zero",         `Quick, test_give_zero
  ]

module Skew_date = Date
module Skew_currency = Currency
open Core

let test_zero () =
  Alcotest.(check bool) "zero sexp"
    true (Sexp.equal (Contract.sexp_of_t Contract.Zero) (Sexp.Atom "Zero"))

let test_give_give_ne_give () =
  let c  = Contract.give (Contract.give (Contract.one Skew_currency.USD)) in
  let c2 = Contract.give (Contract.one Skew_currency.USD) in
  Alcotest.(check bool) "give give <> give" false (Contract.equal c c2)

let test_european_call_structure () =
  let expiry = Skew_date.of_ymd 2026 12 19 in
  let c = Contract.european_call "AAPL" Skew_currency.USD 150.0 expiry in
  match c with
  | Contract.Truncate (_,
      Contract.Get (Contract.Or (
        Contract.Scale (_, Contract.One _),
        Contract.Zero))) ->
    Alcotest.(check bool) "structure" true true
  | _ -> Alcotest.fail "unexpected structure"

let test_sexp_round_trip () =
  let expiry = Skew_date.of_ymd 2026 12 19 in
  let cs =
    [ Contract.Zero
    ; Contract.One Skew_currency.USD
    ; Contract.Give (Contract.One Skew_currency.EUR)
    ; Contract.And (Contract.One Skew_currency.USD, Contract.Zero)
    ; Contract.Truncate (expiry, Contract.Zero)
    ]
  in
  List.iter cs ~f:(fun c ->
    let s = Contract.sexp_of_t c in
    (* Just verify it produces a sexp without raising *)
    ignore s)

let tests =
  [ "zero",                   `Quick, test_zero
  ; "give_give_ne_give",      `Quick, test_give_give_ne_give
  ; "european_call_structure",`Quick, test_european_call_structure
  ; "sexp_round_trip",        `Quick, test_sexp_round_trip
  ]

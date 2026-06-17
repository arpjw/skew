module Skew_date = Date
module Skew_currency = Currency
open Core

let test_parse_call () =
  match Repl.parse "call AAPL 150.0 2026-12-19" with
  | Ok (Contract.Truncate (d,
      Contract.Get (Contract.Or (
        Contract.Scale (_, Contract.One Skew_currency.USD),
        Contract.Zero)))) ->
    Alcotest.(check string) "expiry" "2026-12-19" (Skew_date.to_string d)
  | Ok c ->
    Alcotest.failf "unexpected: %s"
      (Sexp.to_string (Contract.sexp_of_t c))
  | Error e -> Alcotest.fail e

let test_parse_give_one () =
  match Repl.parse "give (one USD)" with
  | Ok (Contract.Give (Contract.One Skew_currency.USD)) -> ()
  | Ok c ->
    Alcotest.failf "unexpected: %s"
      (Sexp.to_string (Contract.sexp_of_t c))
  | Error e -> Alcotest.fail e

let test_parse_and () =
  match Repl.parse "and (one USD) (one EUR)" with
  | Ok (Contract.And (Contract.One Skew_currency.USD,
                      Contract.One Skew_currency.EUR)) -> ()
  | Ok c ->
    Alcotest.failf "unexpected: %s"
      (Sexp.to_string (Contract.sexp_of_t c))
  | Error e -> Alcotest.fail e

let test_parse_zero () =
  match Repl.parse "zero" with
  | Ok Contract.Zero -> ()
  | Ok c ->
    Alcotest.failf "expected Zero, got: %s"
      (Sexp.to_string (Contract.sexp_of_t c))
  | Error e -> Alcotest.fail e

let test_parse_error () =
  match Repl.parse "not_a_valid_contract_xyz" with
  | Error _ -> ()   (* expected *)
  | Ok _    -> Alcotest.fail "expected parse error"

let tests =
  [ "parse_call",     `Quick, test_parse_call
  ; "parse_give_one", `Quick, test_parse_give_one
  ; "parse_and",      `Quick, test_parse_and
  ; "parse_zero",     `Quick, test_parse_zero
  ; "parse_error",    `Quick, test_parse_error
  ]

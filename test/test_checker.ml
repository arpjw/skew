module Skew_currency = Currency
open Core

let ku () = Hashtbl.create (module String)

let test_zero () =
  match Checker.check ~known_underlyings:(ku ()) Contract.Zero with
  | Ok Checker.Void -> ()
  | _ -> Alcotest.fail "expected Void"

let test_one_usd () =
  match Checker.check ~known_underlyings:(ku ()) (Contract.One Skew_currency.USD) with
  | Ok (Checker.Single Skew_currency.USD) -> ()
  | _ -> Alcotest.fail "expected Single USD"

let test_and_same () =
  let c = Contract.And (Contract.One Skew_currency.USD, Contract.One Skew_currency.USD) in
  match Checker.check ~known_underlyings:(ku ()) c with
  | Ok (Checker.Single Skew_currency.USD) -> ()
  | _ -> Alcotest.fail "expected Single USD"

let test_mismatch () =
  let c = Contract.And (Contract.One Skew_currency.USD, Contract.One Skew_currency.EUR) in
  match Checker.check ~known_underlyings:(ku ()) c with
  | Error (_ :: _) -> ()
  | _ -> Alcotest.fail "expected mismatch error"

let test_give () =
  match Checker.check ~known_underlyings:(ku ())
          (Contract.Give (Contract.One Skew_currency.USD)) with
  | Ok (Checker.Single Skew_currency.USD) -> ()
  | _ -> Alcotest.fail "expected Single USD from Give"

let test_void_compat () =
  let c = Contract.And (Contract.Zero, Contract.One Skew_currency.USD) in
  match Checker.check ~known_underlyings:(ku ()) c with
  | Ok (Checker.Single Skew_currency.USD) -> ()
  | _ -> Alcotest.fail "expected Single USD (Zero compatible with anything)"

let tests =
  [ "zero",        `Quick, test_zero
  ; "one_usd",     `Quick, test_one_usd
  ; "and_same",    `Quick, test_and_same
  ; "mismatch",    `Quick, test_mismatch
  ; "give",        `Quick, test_give
  ; "void_compat", `Quick, test_void_compat
  ]

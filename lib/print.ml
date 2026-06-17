module Skew_date = Date
module Skew_currency = Currency
open Core

let rec contract ?(indent = 0) c =
  let sp = String.make (indent * 2) ' ' in
  match c with
  | Contract.Zero -> "Zero"
  | Contract.One ccy ->
    Printf.sprintf "One(%s)" (Skew_currency.to_string ccy)
  | Contract.Give c ->
    Printf.sprintf "Give(\n%s  %s)" sp (contract ~indent:(indent + 1) c)
  | Contract.And (c1, c2) ->
    Printf.sprintf "And(\n%s  %s,\n%s  %s)" sp
      (contract ~indent:(indent + 1) c1) sp
      (contract ~indent:(indent + 1) c2)
  | Contract.Or (c1, c2) ->
    Printf.sprintf "Or(\n%s  %s,\n%s  %s)" sp
      (contract ~indent:(indent + 1) c1) sp
      (contract ~indent:(indent + 1) c2)
  | Contract.Truncate (d, c) ->
    Printf.sprintf "Truncate(%s, %s)" (Skew_date.to_string d)
      (contract ~indent c)
  | Contract.Then (c1, c2) ->
    Printf.sprintf "Then(%s, %s)"
      (contract ~indent:(indent + 1) c1)
      (contract ~indent:(indent + 1) c2)
  | Contract.Scale (obs, c) ->
    Printf.sprintf "Scale(%s, %s)"
      (Observable.float_to_string obs)
      (contract ~indent c)
  | Contract.Get c ->
    Printf.sprintf "Get(%s)" (contract ~indent c)
  | Contract.Anytime c ->
    Printf.sprintf "Anytime(%s)" (contract ~indent c)

(* Try to recognise derived forms and print them nicely *)
let smart_contract c =
  let open Pricer.LatticePricer in
  match identify_contract c with
  | European_call { underlying; strike; expiry } ->
    Printf.sprintf "EuropeanCall(%s, USD, %.2f, %s)"
      underlying strike (Skew_date.to_string expiry)
  | European_put { underlying; strike; expiry } ->
    Printf.sprintf "EuropeanPut(%s, USD, %.2f, %s)"
      underlying strike (Skew_date.to_string expiry)
  | American_call { underlying; strike; expiry } ->
    Printf.sprintf "AmericanCall(%s, USD, %.2f, %s)"
      underlying strike (Skew_date.to_string expiry)
  | American_put { underlying; strike; expiry } ->
    Printf.sprintf "AmericanPut(%s, USD, %.2f, %s)"
      underlying strike (Skew_date.to_string expiry)
  | _ -> contract c

let greeks_report label (r : Greeks.greeks_report) =
  let sep = String.make 54 '-' in
  Printf.sprintf
    "Contract : %s\n%s\nPrice    : %8.4f  (\xc2\xb1%.4f)\n\
     Delta    : %8.4f\nVega     : %8.4f  (per 1 vol pt)\n\
     Theta    : %8.4f  (per day)\nRho      : %8.4f  (per 1bp)"
    label sep
    r.Greeks.price r.Greeks.stderr
    r.Greeks.delta
    r.Greeks.vega
    r.Greeks.theta          (* already per day from greeks.ml *)
    (r.Greeks.rho *. 0.0001)

let check_error = function
  | Checker.CurrencyMismatch { left; right; context } ->
    Printf.sprintf
      "CurrencyMismatch in %s:\n  Left  : %s\n  Right : %s\n  \
       Contracts with different currencies cannot be combined without an FX leg."
      context
      (Sexp.to_string (Checker.sexp_of_currency_type left))
      (Sexp.to_string (Checker.sexp_of_currency_type right))
  | Checker.UnknownUnderlying s ->
    Printf.sprintf "UnknownUnderlying: %s" s
  | Checker.ObservableTypeError s ->
    Printf.sprintf "ObservableTypeError: %s" s

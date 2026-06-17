module Skew_date = Date
module Skew_currency = Currency
open Core

type t =
  | Zero
  | One of Skew_currency.t
  | Give of t
  | And of t * t
  | Or of t * t
  | Truncate of Skew_date.t * t
  | Then of t * t
  | Scale of float Observable.t * t
  | Get of t
  | Anytime of t

(* Manual sexp conversion because Observable.t GADT doesn't support auto-derive *)
let rec sexp_of_t c =
  match c with
  | Zero -> Sexp.Atom "Zero"
  | One ccy -> Sexp.List [Sexp.Atom "One"; Skew_currency.sexp_of_t ccy]
  | Give c -> Sexp.List [Sexp.Atom "Give"; sexp_of_t c]
  | And (c1, c2) -> Sexp.List [Sexp.Atom "And"; sexp_of_t c1; sexp_of_t c2]
  | Or (c1, c2) -> Sexp.List [Sexp.Atom "Or"; sexp_of_t c1; sexp_of_t c2]
  | Truncate (d, c) -> Sexp.List [Sexp.Atom "Truncate"; Skew_date.sexp_of_t d; sexp_of_t c]
  | Then (c1, c2) -> Sexp.List [Sexp.Atom "Then"; sexp_of_t c1; sexp_of_t c2]
  | Scale (obs, c) ->
    Sexp.List [Sexp.Atom "Scale";
               Sexp.Atom (Observable.float_to_string obs);
               sexp_of_t c]
  | Get c -> Sexp.List [Sexp.Atom "Get"; sexp_of_t c]
  | Anytime c -> Sexp.List [Sexp.Atom "Anytime"; sexp_of_t c]

(* Structural equality via sexp comparison *)
let equal c1 c2 = Sexp.equal (sexp_of_t c1) (sexp_of_t c2)

(* Smart constructors *)
let zero = Zero
let one ccy = One ccy
let give c = Give c
let both c1 c2 = And (c1, c2)
let either c1 c2 = Or (c1, c2)
let truncate date c = Truncate (date, c)
let then_ c1 c2 = Then (c1, c2)
let scale obs c = Scale (obs, c)
let get c = Get c
let anytime c = Anytime c

let receive_at ccy date = Truncate (date, Get (One ccy))

let zcb ccy date notional =
  Scale (Observable.konst notional, receive_at ccy date)

let european_call underlying ccy strike expiry =
  Truncate (expiry,
    Get (Or (
      Scale (Observable.(spot underlying -. konst strike), One ccy),
      Zero)))

let european_put underlying ccy strike expiry =
  Truncate (expiry,
    Get (Or (
      Scale (Observable.(konst strike -. spot underlying), One ccy),
      Zero)))

let american_call underlying ccy strike expiry =
  Truncate (expiry, Anytime (
    Or (Scale (Observable.(spot underlying -. konst strike), One ccy), Zero)))

let american_put underlying ccy strike expiry =
  Truncate (expiry, Anytime (
    Or (Scale (Observable.(konst strike -. spot underlying), One ccy), Zero)))

let forward underlying ccy strike expiry =
  Truncate (expiry,
    Get (And (
      Scale (Observable.spot underlying, One ccy),
      Give (Scale (Observable.konst strike, One ccy)))))

let swap fixed_rate floating_underlying ccy payment_dates =
  List.fold payment_dates ~init:Zero ~f:(fun acc date ->
    And (acc,
      And (
        Scale (Observable.spot floating_underlying, receive_at ccy date),
        Give (Scale (Observable.konst fixed_rate, receive_at ccy date)))))

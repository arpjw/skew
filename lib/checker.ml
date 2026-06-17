module Skew_date = Date
module Skew_currency = Currency
open Core

type currency_type =
  | Void
  | Single of Skew_currency.t
  | Multi
[@@deriving sexp]

type check_error =
  | CurrencyMismatch of {
      left    : currency_type;
      right   : currency_type;
      context : string;
    }
  | UnknownUnderlying of string
  | ObservableTypeError of string
[@@deriving sexp]

type check_result = (currency_type, check_error list) Result.t

let merge_currency ct1 ct2 ~context =
  match ct1, ct2 with
  | Void, x | x, Void -> Ok x
  | Single a, Single b when Skew_currency.equal a b -> Ok (Single a)
  | Single _, Single _ ->
    Error [CurrencyMismatch { left = ct1; right = ct2; context }]
  | Multi, _ | _, Multi -> Ok Multi

let check ~known_underlyings:_ contract =
  let rec go = function
    | Contract.Zero -> Ok Void
    | Contract.One ccy -> Ok (Single ccy)
    | Contract.Give c -> go c
    | Contract.And (c1, c2) ->
      (match go c1, go c2 with
       | Ok t1, Ok t2 -> merge_currency t1 t2 ~context:"And"
       | Error e1, Error e2 -> Error (e1 @ e2)
       | Error e, _ | _, Error e -> Error e)
    | Contract.Or (c1, c2) ->
      (match go c1, go c2 with
       | Ok t1, Ok t2 -> merge_currency t1 t2 ~context:"Or"
       | Error e1, Error e2 -> Error (e1 @ e2)
       | Error e, _ | _, Error e -> Error e)
    | Contract.Truncate (_, c) -> go c
    | Contract.Then (c1, c2) ->
      (match go c1, go c2 with
       | Ok t1, Ok t2 -> merge_currency t1 t2 ~context:"Then"
       | Error e1, Error e2 -> Error (e1 @ e2)
       | Error e, _ | _, Error e -> Error e)
    | Contract.Scale (_, c) -> go c
    | Contract.Get c -> go c
    | Contract.Anytime c -> go c
  in
  go contract

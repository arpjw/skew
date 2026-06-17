open Core

type t =
  | USD
  | EUR
  | GBP
  | JPY
  | CHF
  | Other of string
[@@deriving sexp, compare, equal, hash]

let to_string = function
  | USD -> "USD" | EUR -> "EUR" | GBP -> "GBP"
  | JPY -> "JPY" | CHF -> "CHF" | Other s -> s

let of_string = function
  | "USD" -> USD | "EUR" -> EUR | "GBP" -> GBP
  | "JPY" -> JPY | "CHF" -> CHF | s -> Other s

type fx_pair = { base: t; quote: t }
[@@deriving sexp, compare, equal]

let fx_pair base quote = { base; quote }
let flip { base; quote } = { base = quote; quote = base }

open Core

type t = string [@@deriving sexp]

let of_string s = s
let to_string e = e

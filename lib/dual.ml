open Core

type t = { v: float; d: float } [@@deriving sexp]

let make v d = { v; d }
let const x  = { v = x; d = 0.0 }
let var x    = { v = x; d = 1.0 }
let primal t = t.v
let deriv  t = t.d

(* Shorthands for plain float arithmetic to avoid shadowing issues *)
let f_add = Float.( + )
let f_sub = Float.( - )
let f_mul = Float.( * )
let f_div = Float.( / )

(* Dual arithmetic *)
let add a b = { v = f_add a.v b.v; d = f_add a.d b.d }
let sub a b = { v = f_sub a.v b.v; d = f_sub a.d b.d }
let mul a b = { v = f_mul a.v b.v; d = f_add (f_mul a.d b.v) (f_mul a.v b.d) }
let div a b =
  { v = f_div a.v b.v
  ; d = f_div (f_sub (f_mul a.d b.v) (f_mul a.v b.d)) (f_mul b.v b.v)
  }

let ( +. ) = add
let ( -. ) = sub
let ( *. ) = mul
let ( /. ) = div

let neg a = { v = Float.neg a.v; d = Float.neg a.d }
let abs a = if Float.(a.v >= 0.0) then a else neg a

let exp a =
  let e = Float.exp a.v in
  { v = e; d = f_mul a.d e }

let log a =
  { v = Float.log a.v; d = f_div a.d a.v }

let sqrt a =
  let s = Float.sqrt a.v in
  { v = s; d = f_div a.d (f_mul 2.0 s) }

let pow a (b : float) =
  let p = Float.( ** ) a.v b in
  { v = p; d = f_mul a.d (f_mul b (Float.( ** ) a.v (f_sub b 1.0))) }

let max a b = if Float.(a.v >= b.v) then a else b
let min a b = if Float.(a.v <= b.v) then a else b

external erfc_c : float -> float = "caml_erfc_float" "caml_erfc"

let norm_cdf x =
  let v_val = f_mul 0.5 (erfc_c (f_div (Float.neg x.v) (Float.sqrt 2.0))) in
  let phi   = f_div (Float.exp (f_mul (-0.5) (f_mul x.v x.v)))
                    (Float.sqrt (f_mul 2.0 Float.pi)) in
  { v = v_val; d = f_mul x.d phi }

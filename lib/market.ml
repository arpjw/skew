module Skew_date = Date
module Skew_currency = Currency
open Core

type vol_surface = {
  strikes  : float array;
  expiries : float array;
  vols     : float array array;
}

type t = {
  spots          : (string, float) Hashtbl.t;
  rates          : (Skew_currency.t, float) Hashtbl.t;
  vol_surfaces   : (string, vol_surface) Hashtbl.t;
  mutable valuation_date : Skew_date.t;
}

let create ~valuation_date = {
  spots        = Hashtbl.create (module String);
  rates        = Hashtbl.create (module Currency);
  vol_surfaces = Hashtbl.create (module String);
  valuation_date;
}

let set_spot m s v = Hashtbl.set m.spots ~key:s ~data:v
let set_rate m c v = Hashtbl.set m.rates ~key:c ~data:v

let get_spot m s = Hashtbl.find m.spots s

let get_rate m c =
  Option.value (Hashtbl.find m.rates c) ~default:0.0

let set_flat_vol m s v =
  let surf = {
    strikes  = [| 0.0; 1.0e9 |];
    expiries = [| 0.0; 100.0 |];
    vols     = [| [| v; v |]; [| v; v |] |];
  } in
  Hashtbl.set m.vol_surfaces ~key:s ~data:surf

(* Bilinear interpolation on the vol surface *)
let get_vol m s ~strike ~expiry =
  match Hashtbl.find m.vol_surfaces s with
  | None -> 0.2  (* default flat vol *)
  | Some surf ->
    let strikes  = surf.strikes in
    let expiries = surf.expiries in
    let vols     = surf.vols in
    let n_k = Array.length strikes in
    let n_e = Array.length expiries in
    if n_k = 0 || n_e = 0 then 0.2
    else begin
      (* Find bracketing indices with clamping *)
      let find_bracket (arr : float array) (x : float) =
        let n    = Array.length arr in
        let last = n - 1 in
        if n = 1 then (0, 0, 0.0)
        else if Float.(x <= arr.(0)) then (0, 0, 0.0)
        else if Float.(x >= arr.(last)) then (last, last, 0.0)
        else begin
          let i = ref 0 in
          while !i < last - 1
                && (let j = !i + 1 in Float.(arr.(j) <= x))
          do incr i done;
          let lo = !i and hi = !i + 1 in
          let t = (x -. arr.(lo)) /. (arr.(hi) -. arr.(lo)) in
          (lo, hi, t)
        end
      in
      let ki, _kj, kt = find_bracket strikes strike in
      let ei, _ej, et = find_bracket expiries expiry in
      let ki2 = min (ki + 1) (n_k - 1) in
      let ei2 = min (ei + 1) (n_e - 1) in
      let v00 = vols.(ki).(ei) in
      let v10 = vols.(ki2).(ei) in
      let v01 = vols.(ki).(ei2) in
      let v11 = vols.(ki2).(ei2) in
      let v0 = v00 *. (1.0 -. kt) +. v10 *. kt in
      let v1 = v01 *. (1.0 -. kt) +. v11 *. kt in
      v0 *. (1.0 -. et) +. v1 *. et
    end

let to_obs_env (m : t) cur_date : Observable.env =
  { Observable.spots    = Hashtbl.copy m.spots
  ; rates               = Hashtbl.copy m.rates
  ; cur_date
  }

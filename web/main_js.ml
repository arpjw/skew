open Js_of_ocaml

(* Alias Skew's Date module before Core shadows it *)
module Skew_date = Date

open Core

(* Global market state — use a fixed date since Core_unix.gettimeofday
   may not be available in js_of_ocaml *)
let market = ref (Market.create ~valuation_date:(Skew_date.of_ymd 2025 1 1))

let () =
  Market.set_rate !market Currency.USD 0.05

let () =
  Js.export "Skew"
    (object%js
      method setSpot underlying price =
        let u = Js.to_string underlying in
        let p = Js.float_of_number price in
        Market.set_spot !market u p

      method setVol underlying vol =
        let u = Js.to_string underlying in
        let v = Js.float_of_number vol in
        Market.set_flat_vol !market u v

      method setRate ccy rate =
        let c = Currency.of_string (Js.to_string ccy) in
        let r = Js.float_of_number rate in
        Market.set_rate !market c r

      method check expr_js =
        let expr = Js.to_string expr_js in
        (match Repl.parse expr with
         | Error e ->
           object%js
             val ok = Js._false
             val error = Js.string e
             val _type = Js.string ""
           end
         | Ok c ->
           let ku = Hashtbl.create (module String) in
           (match Checker.check ~known_underlyings:ku c with
            | Error errs ->
              let msg = List.map errs ~f:Print.check_error |> String.concat ~sep:"; " in
              object%js
                val ok = Js._false
                val error = Js.string msg
                val _type = Js.string ""
              end
            | Ok ct ->
              object%js
                val ok = Js._true
                val error = Js.string ""
                val _type = Js.string (Sexp.to_string (Checker.sexp_of_currency_type ct))
              end))

      method simplify expr_js =
        let expr = Js.to_string expr_js in
        let simplified = match Repl.parse expr with
          | Error _ -> expr
          | Ok c -> Print.contract (Simplify.simplify c)
        in
        object%js
          val simplified = Js.string simplified
        end

      method price expr_js =
        let expr = Js.to_string expr_js in
        (match Repl.parse expr with
         | Error _ ->
           object%js
             val price = Js.number_of_float 0.0
             val stderr = Js.number_of_float 0.0
           end
         | Ok c ->
           let config = Pricer.MonteCarlo.default_config in
           let p, se = Pricer.MonteCarlo.price_with_stderr ~config ~market:!market ~contract:c in
           object%js
             val price = Js.number_of_float p
             val stderr = Js.number_of_float se
           end)

      method lattice expr_js =
        let expr = Js.to_string expr_js in
        (match Repl.parse expr with
         | Error _ ->
           object%js val price = Js.number_of_float 0.0 end
         | Ok c ->
           let config = Pricer.LatticePricer.default_config in
           let p = Pricer.LatticePricer.price ~config ~market:!market ~contract:c in
           object%js val price = Js.number_of_float p end)

      method greeks expr_js underlying_js =
        let expr = Js.to_string expr_js in
        let underlying = Js.to_string underlying_js in
        (match Repl.parse expr with
         | Error _ ->
           object%js
             val price = Js.number_of_float 0.0
             val delta = Js.number_of_float 0.0
             val vega  = Js.number_of_float 0.0
             val theta = Js.number_of_float 0.0
             val rho   = Js.number_of_float 0.0
             val stderr = Js.number_of_float 0.0
           end
         | Ok c ->
           let r = Greeks.compute_report ~market:!market ~contract:c ~underlying in
           object%js
             val price  = Js.number_of_float r.Greeks.price
             val delta  = Js.number_of_float r.Greeks.delta
             val vega   = Js.number_of_float r.Greeks.vega
             val theta  = Js.number_of_float r.Greeks.theta
             val rho    = Js.number_of_float r.Greeks.rho
             val stderr = Js.number_of_float r.Greeks.stderr
           end)
    end)

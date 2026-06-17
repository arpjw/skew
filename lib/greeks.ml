module Skew_date = Date
module Skew_currency = Currency
open Core

(* Evaluate a float observable using dual numbers.
   We infer arithmetic operations from sample evaluation. *)
(* Evaluate a float observable using dual numbers.
   Strategy: use finite differences to compute the gradient w.r.t. all spots.
   For each spot s, compute dV/dS_s by perturbing the env, then combine via chain rule. *)
let eval_float_dual
    (obs : float Observable.t)
    ~(env : Observable.env)
    ~(spot_dual : string -> Dual.t)
  : Dual.t =
  (* Compute primal *)
  let primal = Observable.eval env obs in
  (* For each spot in the env, check if perturbing it changes the observable *)
  let deriv = Hashtbl.fold env.Observable.spots ~init:0.0
    ~f:(fun ~key:spot_name ~data:s0 acc ->
      let d_param = (spot_dual spot_name).Dual.d in
      if Float.(d_param = 0.0) then acc
      else begin
        let eps = Float.max (Float.abs s0 *. 1e-5) 1e-5 in
        let env_up = { env with
          Observable.spots = Hashtbl.copy env.Observable.spots } in
        Hashtbl.set env_up.Observable.spots ~key:spot_name ~data:(s0 +. eps);
        let v_up = Observable.eval env_up obs in
        let dV_dS = (v_up -. primal) /. eps in
        acc +. dV_dS *. d_param
      end)
  in
  Dual.make primal deriv

(* Copy a market (shallow-copy hashtables) *)
let copy_market (m : Market.t) =
  let m2 = Market.create ~valuation_date:m.Market.valuation_date in
  Hashtbl.iteri m.Market.spots ~f:(fun ~key ~data ->
    Market.set_spot m2 key data);
  Hashtbl.iteri m.Market.rates ~f:(fun ~key ~data ->
    Market.set_rate m2 key data);
  Hashtbl.iteri m.Market.vol_surfaces ~f:(fun ~key ~data ->
    (* Copy by reading off the [0][0] entry as a flat vol *)
    let v =
      if Array.length data.Market.vols > 0
         && Array.length data.Market.vols.(0) > 0
      then data.Market.vols.(0).(0)
      else 0.2
    in
    Market.set_flat_vol m2 key v);
  m2

(* Pathwise delta via dual-number differentiation *)
let delta ~market ~contract ~underlying ~n_paths =
  let config =
    Pricer.MonteCarlo.{ default_config with n_paths; seed = Some 42 }
  in
  let rng = Path.create_rng 42 in
  let underlyings =
    Pricer.extract_underlyings contract
    |> List.dedup_and_sort ~compare:String.compare
  in
  let eval_dates =
    Pricer.build_eval_dates market contract config.Pricer.MonteCarlo.n_steps
  in
  let spot0 = Option.value (Market.get_spot market underlying) ~default:100.0 in
  let val_date = market.Market.valuation_date in
  let total_d = ref 0.0 in
  for _ = 1 to n_paths do
    let scenario =
      Path.simulate_scenario ~rng ~market ~underlyings ~eval_dates
    in
    (* eval_c evaluates contract at given date using path prices,
       tracking derivative w.r.t. initial spot via dual numbers. *)
    let make_env_at date =
      let env = Market.to_obs_env market date in
      (* Override spots with path prices at the given date *)
      Hashtbl.iteri scenario.Path.paths ~f:(fun ~key:u ~data:p ->
        let path_price = Path.price_at_date p date in
        Hashtbl.set env.Observable.spots ~key:u ~data:path_price);
      env
    in
    (* spot_dual at expiry date: maps initial S perturbation to terminal S perturbation *)
    let make_spot_dual env s =
      if String.(s = underlying) then begin
        (* d(S_T)/d(S_0) ≈ (S_T_up - S_T) / eps via the path itself
           Approximation: use S_T / S_0 * 1.0 via dual (pathwise delta approach)
           Actually for GBM, dS_T/dS_0 = S_T/S_0 (log-linearity) *)
        let path_spot = Option.value (Hashtbl.find env.Observable.spots s) ~default:spot0 in
        (* Chain rule: d(payoff)/d(S_0) = d(payoff)/d(S_T) * d(S_T)/d(S_0)
           For GBM: d(S_T)/d(S_0) = S_T / S_0
           So set dual part = S_T / S_0 *)
        Dual.make path_spot (path_spot /. spot0)
      end else
        let sv = Option.value (Hashtbl.find env.Observable.spots s) ~default:spot0 in
        Dual.const sv
    in
    let rec eval_c date c =
      match c with
      | Contract.Zero     -> Dual.const 0.0
      | Contract.One ccy  ->
        let r = Market.get_rate market ccy in
        let t = Skew_date.year_frac Skew_date.Act365 val_date date in
        Dual.const (Float.exp (-. r *. t))
      | Contract.Give c   -> Dual.neg (eval_c date c)
      | Contract.And  (c1, c2) -> Dual.(eval_c date c1 +. eval_c date c2)
      | Contract.Or   (c1, c2) -> Dual.max (eval_c date c1) (eval_c date c2)
      | Contract.Truncate (expiry, c) ->
        (* Evaluate inner at the expiry date *)
        eval_c expiry c
      | Contract.Then (c1, c2)   -> Dual.(eval_c date c1 +. eval_c date c2)
      | Contract.Scale (obs, c) ->
        let env = make_env_at date in
        let spot_dual = make_spot_dual env in
        let ov = eval_float_dual obs ~env ~spot_dual in
        Dual.(ov *. eval_c date c)
      | Contract.Get     c -> eval_c date c
      | Contract.Anytime c -> eval_c date c
    in
    let result = eval_c val_date contract in
    total_d := !total_d +. result.Dual.d
  done;
  !total_d /. Float.of_int n_paths

(* Bump-and-reval for vega *)
let vega ~market ~contract ~underlying ~bump ~n_paths =
  let config =
    Pricer.MonteCarlo.{ default_config with n_paths; seed = Some 42 }
  in
  let m_up = copy_market market in
  let m_dn = copy_market market in
  let v0 = Market.get_vol market underlying ~strike:100.0 ~expiry:1.0 in
  Market.set_flat_vol m_up underlying (v0 +. bump);
  Market.set_flat_vol m_dn underlying (v0 -. bump);
  let p_up = Pricer.MonteCarlo.price ~config ~market:m_up ~contract in
  let p_dn = Pricer.MonteCarlo.price ~config ~market:m_dn ~contract in
  (p_up -. p_dn) /. (2.0 *. bump)

(* Bump-and-reval for theta (forward difference, 1-day bump) *)
let theta ~market ~contract ~n_paths =
  let config =
    Pricer.MonteCarlo.{ default_config with n_paths; seed = Some 42 }
  in
  let m_tomorrow =
    Market.create
      ~valuation_date:(Skew_date.add_days market.Market.valuation_date 1)
  in
  Hashtbl.iteri market.Market.spots ~f:(fun ~key ~data ->
    Market.set_spot m_tomorrow key data);
  Hashtbl.iteri market.Market.rates ~f:(fun ~key ~data ->
    Market.set_rate m_tomorrow key data);
  Hashtbl.iteri market.Market.vol_surfaces ~f:(fun ~key ~data ->
    let v =
      if Array.length data.Market.vols > 0
         && Array.length data.Market.vols.(0) > 0
      then data.Market.vols.(0).(0)
      else 0.2
    in
    Market.set_flat_vol m_tomorrow key v);
  let p0 = Pricer.MonteCarlo.price ~config ~market         ~contract in
  let p1 = Pricer.MonteCarlo.price ~config ~market:m_tomorrow ~contract in
  (* Theta = change per day (negative for long options) *)
  p1 -. p0

(* Bump-and-reval for rho (central difference) *)
let rho ~market ~contract ~currency ~bump ~n_paths =
  let config =
    Pricer.MonteCarlo.{ default_config with n_paths; seed = Some 42 }
  in
  let m_up = copy_market market in
  let m_dn = copy_market market in
  let r0 = Market.get_rate market currency in
  Market.set_rate m_up currency (r0 +. bump);
  Market.set_rate m_dn currency (r0 -. bump);
  let p_up = Pricer.MonteCarlo.price ~config ~market:m_up ~contract in
  let p_dn = Pricer.MonteCarlo.price ~config ~market:m_dn ~contract in
  (p_up -. p_dn) /. (2.0 *. bump)

type greeks_report = {
  price  : float;
  delta  : float;
  vega   : float;
  theta  : float;
  rho    : float;
  stderr : float;
} [@@deriving sexp]

let compute_report ~n_paths ~market ~contract ~underlying =
  let config  = Pricer.MonteCarlo.{ default_config with n_paths } in
  let price, stderr =
    Pricer.MonteCarlo.price_with_stderr ~config ~market ~contract
  in
  let delta = delta ~market ~contract ~underlying ~n_paths in
  let vega  = vega  ~market ~contract ~underlying ~bump:0.01 ~n_paths in
  let theta = theta ~market ~contract ~n_paths in
  let rho   = rho   ~market ~contract ~currency:Skew_currency.USD
                ~bump:0.0001 ~n_paths
  in
  { price; delta; vega; theta; stderr; rho }

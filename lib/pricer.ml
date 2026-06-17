module Skew_date = Date
module Skew_currency = Currency
open Core

module type PRICER = sig
  type config
  val default_config : config
  val price
    :  config:config
    -> market:Market.t
    -> contract:Contract.t
    -> float
  val price_with_stderr
    :  config:config
    -> market:Market.t
    -> contract:Contract.t
    -> float * float
end

(* Walk contract tree collecting Spot underlying names *)
let rec extract_underlyings_obs : type a. a Observable.t -> string list = function
  | Observable.Spot s -> [s]
  | Observable.Const _ -> []
  | Observable.Lift1 (_, a) -> extract_underlyings_obs a
  | Observable.Lift2 (_, a, b) ->
    extract_underlyings_obs a @ extract_underlyings_obs b
  | Observable.Date -> []
  | Observable.Horizon _ -> []
  | Observable.Rate _ -> []
  | Observable.Greater (a, b) ->
    extract_underlyings_obs a @ extract_underlyings_obs b
  | Observable.Equal (a, b) ->
    extract_underlyings_obs a @ extract_underlyings_obs b
  | Observable.If (c, t, f) ->
    extract_underlyings_obs c
    @ extract_underlyings_obs t
    @ extract_underlyings_obs f

let rec extract_underlyings = function
  | Contract.Zero | Contract.One _ -> []
  | Contract.Give c -> extract_underlyings c
  | Contract.And (c1, c2)
  | Contract.Or  (c1, c2)
  | Contract.Then (c1, c2) ->
    extract_underlyings c1 @ extract_underlyings c2
  | Contract.Truncate (_, c)
  | Contract.Get c
  | Contract.Anytime c ->
    extract_underlyings c
  | Contract.Scale (obs, c) ->
    extract_underlyings_obs obs @ extract_underlyings c

let rec latest_expiry = function
  | Contract.Zero | Contract.One _ -> None
  | Contract.Give c -> latest_expiry c
  | Contract.And (c1, c2)
  | Contract.Or  (c1, c2)
  | Contract.Then (c1, c2) ->
    (match latest_expiry c1, latest_expiry c2 with
     | None, x | x, None -> x
     | Some a, Some b -> Some (if a > b then a else b))
  | Contract.Truncate (d, _) -> Some d
  | Contract.Scale (_, c)
  | Contract.Get c
  | Contract.Anytime c ->
    latest_expiry c

let build_eval_dates (market : Market.t) contract n_steps =
  let start = market.valuation_date in
  match latest_expiry contract with
  | None ->
    let end_ = Skew_date.add_days start 365 in
    let total = Skew_date.diff_days end_ start in
    Array.init n_steps ~f:(fun i ->
      Skew_date.add_days start (i * total / (max 1 (n_steps - 1))))
  | Some end_ ->
    let total = Skew_date.diff_days end_ start in
    if total <= 0 then [| start |]
    else
      Array.init n_steps ~f:(fun i ->
        Skew_date.add_days start (i * total / (max 1 (n_steps - 1))))

(* ────────────────────────────────────────────── Monte Carlo ── *)
module MonteCarlo = struct
  type config = {
    n_paths   : int;
    n_steps   : int;
    seed      : int option;
    day_count : Skew_date.day_count;
    discount  : bool;
  }

  let default_config = {
    n_paths   = 10_000;
    n_steps   = 252;
    seed      = Some 42;
    discount  = true;
    day_count = Skew_date.Act365;
  }

  let eval_contract_on_scenario ~market ~contract ~scenario ~config =
    let val_date = market.Market.valuation_date in
    (* eval_at date c: evaluate contract c with cashflows at 'date',
       discounting back to val_date *)
    let rec eval_at date c =
      match c with
      | Contract.Zero -> 0.0
      | Contract.One ccy ->
        let r = Market.get_rate market ccy in
        let t = Skew_date.year_frac config.day_count val_date date in
        if config.discount then Float.exp (-. r *. t) else 1.0
      | Contract.Give c -> -. (eval_at date c)
      | Contract.And  (c1, c2) -> eval_at date c1 +. eval_at date c2
      | Contract.Or   (c1, c2) -> Float.max (eval_at date c1) (eval_at date c2)
      | Contract.Truncate (expiry, c) ->
        (* Evaluate the inner contract at the expiry date *)
        eval_at expiry c
      | Contract.Then (c1, c2) -> eval_at date c1 +. eval_at date c2
      | Contract.Scale (obs, c) ->
        let v = Path.eval_obs_on_path ~scenario ~market ~date obs in
        v *. eval_at date c
      | Contract.Get     c -> eval_at date c
      | Contract.Anytime c -> eval_at date c
    in
    eval_at val_date contract

  let price_with_stderr ~config ~market ~contract =
    let rng = match config.seed with
      | Some s -> Path.create_rng s
      | None   -> Path.default_rng ()
    in
    let underlyings =
      extract_underlyings contract
      |> List.dedup_and_sort ~compare:String.compare
    in
    let eval_dates = build_eval_dates market contract config.n_steps in
    let total    = ref 0.0 in
    let total_sq = ref 0.0 in
    let n = config.n_paths in
    for _ = 1 to n do
      let scenario =
        Path.simulate_scenario ~rng ~market ~underlyings ~eval_dates in
      let pv =
        eval_contract_on_scenario ~market ~contract ~scenario ~config in
      total    := !total    +. pv;
      total_sq := !total_sq +. pv *. pv
    done;
    let mean     = !total /. Float.of_int n in
    let variance = !total_sq /. Float.of_int n -. mean *. mean in
    let stderr   = Float.sqrt (Float.max 0.0 variance /. Float.of_int n) in
    (mean, stderr)

  let price ~config ~market ~contract =
    fst (price_with_stderr ~config ~market ~contract)
end

(* ────────────────────────────────────────────── Lattice ── *)
module LatticePricer = struct
  type config = {
    n_steps   : int;
    day_count : Skew_date.day_count;
  }

  let default_config = {
    n_steps   = 500;
    day_count = Skew_date.Act365;
  }

  type option_params = { underlying: string; strike: float; expiry: Skew_date.t }

  type contract_kind =
    | European_call of option_params
    | European_put  of option_params
    | American_call of option_params
    | American_put  of option_params
    | Zero_contract
    | Scaled of float * Contract.t
    | Given  of Contract.t
    | Unsupported

  (* Parse the option type from the string representation of the observable.
     call: "(Spot(X) op C)"  where Spot comes first
     put:  "(C op Spot(X))"  where Const comes first *)
  let parse_option_obs (obs : float Observable.t)
    : (string * float * bool) option =
    let s = Observable.float_to_string obs in
    (* Try to extract Spot name and constant from the string form *)
    match String.chop_prefix s ~prefix:"(Spot(" with
    | Some rest ->
      (* "(Spot(U) op K)" - call form *)
      (match String.lsplit2 rest ~on:')' with
       | Some (underlying, rest2) ->
         let rest2 = String.strip rest2 in
         (* rest2 should be like " op K)" *)
         let last = String.length rest2 - 1 in
         let rest2 = if last >= 0 && Char.equal rest2.[last] ')' then
           String.sub rest2 ~pos:0 ~len:last else rest2 in
         let parts = String.split rest2 ~on:' ' in
         (match List.filter parts ~f:(fun s -> not (String.is_empty s)) with
          | [_op; k_str] ->
            (try Some (underlying, Float.of_string k_str, true)
             with _ -> None)
          | _ -> None)
       | None -> None)
    | None ->
      (* Try put form: "(K op Spot(U))" *)
      (match String.chop_prefix s ~prefix:"(" with
       | Some rest ->
         (* Find "Spot(" in rest *)
         (match String.substr_index rest ~pattern:"Spot(" with
          | Some idx when idx > 2 ->
            (* k_str is before the first space *)
            let before_spot = String.sub rest ~pos:0 ~len:(idx - 1) in
            let parts = String.split before_spot ~on:' ' in
            let parts = List.filter parts ~f:(fun s -> not (String.is_empty s)) in
            (match parts with
             | [k_str; _] | [k_str] ->
               (match String.substr_index rest ~pattern:"Spot(" with
                | Some i ->
                  let after = String.sub rest ~pos:(i + 5) ~len:(String.length rest - i - 5) in
                  (match String.lsplit2 after ~on:')' with
                   | Some (underlying, _) ->
                     (try Some (underlying, Float.of_string k_str, false)
                      with _ -> None)
                   | None -> None)
                | None -> None)
             | _ -> None)
          | _ -> None)
       | None -> None)

  let identify_contract (c : Contract.t) : contract_kind =
    match c with
    | Contract.Zero -> Zero_contract
    | Contract.Give inner -> Given inner
    | Contract.Scale (Observable.Const k, inner) -> Scaled (k, inner)
    (* European option: Truncate(e, Get(Or(Scale(payoff_obs, One _), Zero))) *)
    | Contract.Truncate (expiry,
        Contract.Get (Contract.Or (
          Contract.Scale (payoff_obs, Contract.One _),
          Contract.Zero))) ->
      (match parse_option_obs payoff_obs with
       | Some (u, k, true)  -> European_call { underlying = u; strike = k; expiry }
       | Some (u, k, false) -> European_put  { underlying = u; strike = k; expiry }
       | None -> Unsupported)
    (* American option: Truncate(e, Anytime(Or(Scale(payoff_obs, One _), Zero))) *)
    | Contract.Truncate (expiry,
        Contract.Anytime (Contract.Or (
          Contract.Scale (payoff_obs, Contract.One _),
          Contract.Zero))) ->
      (match parse_option_obs payoff_obs with
       | Some (u, k, true)  -> American_call { underlying = u; strike = k; expiry }
       | Some (u, k, false) -> American_put  { underlying = u; strike = k; expiry }
       | None -> Unsupported)
    | _ -> Unsupported

  let rec price ~config ~market ~contract =
    match identify_contract contract with
    | Zero_contract -> 0.0
    | Given inner -> -. (price ~config ~market ~contract:inner)
    | Scaled (k, inner) -> k *. (price ~config ~market ~contract:inner)
    | European_call { underlying; strike; expiry } ->
      let spot = Option.value (Market.get_spot market underlying) ~default:100.0 in
      let vol  = Market.get_vol market underlying ~strike ~expiry:1.0 in
      let rate = Market.get_rate market Skew_currency.USD in
      let t    = Skew_date.year_frac config.day_count market.Market.valuation_date expiry in
      Lattice.price_european_call ~spot ~vol ~rate ~t_years:t
        ~n_steps:config.n_steps ~strike
    | European_put { underlying; strike; expiry } ->
      let spot = Option.value (Market.get_spot market underlying) ~default:100.0 in
      let vol  = Market.get_vol market underlying ~strike ~expiry:1.0 in
      let rate = Market.get_rate market Skew_currency.USD in
      let t    = Skew_date.year_frac config.day_count market.Market.valuation_date expiry in
      Lattice.price_european_put ~spot ~vol ~rate ~t_years:t
        ~n_steps:config.n_steps ~strike
    | American_call { underlying; strike; expiry } ->
      let spot = Option.value (Market.get_spot market underlying) ~default:100.0 in
      let vol  = Market.get_vol market underlying ~strike ~expiry:1.0 in
      let rate = Market.get_rate market Skew_currency.USD in
      let t    = Skew_date.year_frac config.day_count market.Market.valuation_date expiry in
      Lattice.price_american_call ~spot ~vol ~rate ~t_years:t
        ~n_steps:config.n_steps ~strike
    | American_put { underlying; strike; expiry } ->
      let spot = Option.value (Market.get_spot market underlying) ~default:100.0 in
      let vol  = Market.get_vol market underlying ~strike ~expiry:1.0 in
      let rate = Market.get_rate market Skew_currency.USD in
      let t    = Skew_date.year_frac config.day_count market.Market.valuation_date expiry in
      Lattice.price_american_put ~spot ~vol ~rate ~t_years:t
        ~n_steps:config.n_steps ~strike
    | Unsupported ->
      Printf.eprintf "Lattice: unsupported contract form, falling back to MC\n%!";
      MonteCarlo.(price ~config:default_config ~market ~contract)

  let price_with_stderr ~config ~market ~contract =
    (price ~config ~market ~contract, 0.0)
end

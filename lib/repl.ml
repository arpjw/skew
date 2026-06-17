module Skew_date = Date
module Skew_currency = Currency
open Core

(* ──────────────────────────────── Tokeniser ── *)

type token =
  | Word of string
  | Lparen
  | Rparen
  | EOF

let tokenize s =
  let tokens = ref [] in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    match s.[!i] with
    | ' ' | '\t' | '\n' -> incr i
    | '(' -> tokens := Lparen :: !tokens; incr i
    | ')' -> tokens := Rparen :: !tokens; incr i
    | _ ->
      let start = !i in
      while !i < n
            && not (Char.equal s.[!i] ' ')
            && not (Char.equal s.[!i] '(')
            && not (Char.equal s.[!i] ')') do
        incr i
      done;
      tokens := Word (String.sub s ~pos:start ~len:(!i - start)) :: !tokens
  done;
  List.rev !tokens @ [EOF]

(* ──────────────────────────────── Parser ── *)

type parse_state = { mutable tokens: token list }

let peek  ps = match ps.tokens with t :: _ -> t | [] -> EOF
let advance ps = match ps.tokens with _ :: rest -> ps.tokens <- rest | [] -> ()
let consume ps = let t = peek ps in advance ps; t

let parse_ccy = function
  | "USD" -> Skew_currency.USD | "EUR" -> Skew_currency.EUR
  | "GBP" -> Skew_currency.GBP | "JPY" -> Skew_currency.JPY
  | "CHF" -> Skew_currency.CHF | s -> Skew_currency.Other s

let expect_word ps what =
  match consume ps with
  | Word w -> w
  | _ -> failwith ("expected " ^ what)

let expect_float ps what =
  Float.of_string (expect_word ps what)

let expect_date ps what =
  Skew_date.of_string (expect_word ps what)

let rec parse_expr ps =
  match peek ps with
  | Lparen ->
    advance ps;
    let c = parse_expr ps in
    (match peek ps with Rparen -> advance ps | _ -> ());
    c
  | Word w ->
    advance ps;
    (match String.lowercase w with
     | "zero" -> Contract.Zero
     | "one"  ->
       let ccy_str = expect_word ps "currency" in
       Contract.One (parse_ccy ccy_str)
     | "give"    -> Contract.Give (parse_expr ps)
     | "and"     ->
       let c1 = parse_expr ps in
       let c2 = parse_expr ps in
       Contract.And (c1, c2)
     | "or"      ->
       let c1 = parse_expr ps in
       let c2 = parse_expr ps in
       Contract.Or (c1, c2)
     | "get"     -> Contract.Get (parse_expr ps)
     | "anytime" -> Contract.Anytime (parse_expr ps)
     | "truncate" ->
       let d = expect_date ps "date" in
       Contract.Truncate (d, parse_expr ps)
     | "scale" ->
       let obs = parse_obs ps in
       Contract.Scale (obs, parse_expr ps)
     | "call" ->
       let u = expect_word  ps "underlying" in
       let k = expect_float ps "strike" in
       let e = expect_date  ps "expiry" in
       Contract.european_call u Skew_currency.USD k e
     | "put" ->
       let u = expect_word  ps "underlying" in
       let k = expect_float ps "strike" in
       let e = expect_date  ps "expiry" in
       Contract.european_put u Skew_currency.USD k e
     | "acall" ->
       let u = expect_word  ps "underlying" in
       let k = expect_float ps "strike" in
       let e = expect_date  ps "expiry" in
       Contract.american_call u Skew_currency.USD k e
     | "aput" ->
       let u = expect_word  ps "underlying" in
       let k = expect_float ps "strike" in
       let e = expect_date  ps "expiry" in
       Contract.american_put u Skew_currency.USD k e
     | "forward" ->
       let u = expect_word  ps "underlying" in
       let k = expect_float ps "strike" in
       let e = expect_date  ps "expiry" in
       Contract.forward u Skew_currency.USD k e
     | "zcb" ->
       let ccy = parse_ccy (expect_word ps "currency") in
       let d   = expect_date  ps "date" in
       let n   = expect_float ps "notional" in
       Contract.zcb ccy d n
     | _ -> failwith ("unknown contract keyword: " ^ w))
  | t ->
    let tok_str = match t with
      | Word w  -> "'" ^ w ^ "'"
      | Lparen  -> "'('"
      | Rparen  -> "')'"
      | EOF     -> "EOF"
    in
    failwith ("unexpected token " ^ tok_str)

and parse_obs ps =
  match peek ps with
  | Word "spot" ->
    advance ps;
    let s = expect_word ps "underlying name" in
    Observable.Spot s
  | Word w ->
    advance ps;
    (try Observable.Const (Float.of_string w)
     with _ -> failwith ("expected observable, got: " ^ w))
  | _ -> failwith "expected observable (float or 'spot <name>')"

let parse s =
  let ps = { tokens = tokenize s } in
  match parse_expr ps with
  | c -> Ok c
  | exception exn -> Error (Exn.to_string exn)

(* ──────────────────────────────── Command dispatch ── *)

let dispatch market line =
  let parts =
    String.split ~on:' ' (String.strip line)
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  match parts with
  | [] -> ()
  | cmd :: rest ->
    let rest_str = String.concat ~sep:" " rest in
    (match cmd with
     | ":help" ->
       List.iter ~f:print_endline
         [ "Commands:"
         ; "  :price <expr>               -- price a contract (Monte Carlo)"
         ; "  :check <expr>               -- currency type-check"
         ; "  :simplify <expr>            -- normalize contract"
         ; "  :greeks <expr> <underlying> -- compute full Greeks report"
         ; "  :lattice <expr>             -- price using lattice backend"
         ; "  :both <expr>               -- price with both backends"
         ; "  :set spot <name> <value>    -- update spot price"
         ; "  :set vol  <name> <value>    -- update flat vol"
         ; "  :set rate <ccy>  <value>    -- update risk-free rate"
         ; "  :show market                -- display current market"
         ; "  :show contract <expr>       -- pretty-print contract tree"
         ; "  :quit                       -- exit"
         ]

     | ":set" ->
       (match rest with
        | ["spot"; u; v] ->
          Market.set_spot market u (Float.of_string v);
          Printf.printf "Market: %s spot = %.4f\n" u (Float.of_string v)
        | ["vol"; u; v] ->
          Market.set_flat_vol market u (Float.of_string v);
          Printf.printf "Market: %s vol = %.4f\n" u (Float.of_string v)
        | ["rate"; c; v] ->
          Market.set_rate market (Skew_currency.of_string c) (Float.of_string v);
          Printf.printf "Market: %s rate = %.2f%%\n" c
            (Float.of_string v *. 100.0)
        | _ ->
          print_endline "Usage: :set spot|vol|rate <name> <value>")

     | ":show" ->
       (match rest with
        | ["market"] ->
          print_endline "Spots:";
          Hashtbl.iteri market.Market.spots ~f:(fun ~key ~data ->
            Printf.printf "  %s = %.4f\n" key data);
          print_endline "Rates:";
          Hashtbl.iteri market.Market.rates ~f:(fun ~key ~data ->
            Printf.printf "  %s = %.4f\n" (Skew_currency.to_string key) data)
        | "contract" :: expr_parts ->
          (match parse (String.concat ~sep:" " expr_parts) with
           | Ok c  -> print_endline (Print.contract c)
           | Error e -> Printf.printf "Parse error: %s\n" e)
        | _ -> print_endline "Usage: :show market|contract <expr>")

     | ":price" ->
       (match parse rest_str with
        | Error e -> Printf.printf "Parse error: %s\n" e
        | Ok c ->
          let config = Pricer.MonteCarlo.default_config in
          let p, se =
            Pricer.MonteCarlo.price_with_stderr ~config ~market ~contract:c
          in
          Printf.printf "Price    : %8.4f  (\xc2\xb1%.4f, N=%d)\n"
            p se config.n_paths)

     | ":lattice" ->
       (match parse rest_str with
        | Error e -> Printf.printf "Parse error: %s\n" e
        | Ok c ->
          let config = Pricer.LatticePricer.default_config in
          let p = Pricer.LatticePricer.price ~config ~market ~contract:c in
          Printf.printf "Price    : %8.4f  (N=%d steps)\n" p config.n_steps)

     | ":both" ->
       (match parse rest_str with
        | Error e -> Printf.printf "Parse error: %s\n" e
        | Ok c ->
          let mc_cfg  = Pricer.MonteCarlo.default_config in
          let lat_cfg = Pricer.LatticePricer.default_config in
          let mc_p, mc_se =
            Pricer.MonteCarlo.price_with_stderr ~config:mc_cfg ~market
              ~contract:c
          in
          let lat_p =
            Pricer.LatticePricer.price ~config:lat_cfg ~market ~contract:c
          in
          Printf.printf "MC     : %8.4f  (\xc2\xb1%.4f)\n" mc_p mc_se;
          Printf.printf "Lattice: %8.4f\n" lat_p;
          Printf.printf "Diff   : %8.4f  (%.2f%%%%)\n"
            (Float.abs (mc_p -. lat_p))
            (if Float.(mc_p <> 0.0)
             then Float.abs (mc_p -. lat_p) /. Float.abs mc_p *. 100.0
             else 0.0))

     | ":check" ->
       (match parse rest_str with
        | Error e -> Printf.printf "Parse error: %s\n" e
        | Ok c ->
          let known = Hashtbl.create (module String) in
          (match Checker.check ~known_underlyings:known c with
           | Ok ct ->
             Printf.printf "\xe2\x9c\x93 %s\n"
               (Sexp.to_string (Checker.sexp_of_currency_type ct))
           | Error errs ->
             List.iter errs ~f:(fun e ->
               print_endline (Print.check_error e))))

     | ":simplify" ->
       (match parse rest_str with
        | Error e -> Printf.printf "Parse error: %s\n" e
        | Ok c ->
          Printf.printf "Before : %s\n" (Print.contract c);
          let s = Simplify.simplify c in
          Printf.printf "After  : %s\n" (Print.contract s))

     | ":greeks" ->
       let n = List.length rest in
       if n < 2 then
         print_endline "Usage: :greeks <expr> <underlying>"
       else begin
         let underlying = List.last_exn rest in
         let expr = String.concat ~sep:" " (List.take rest (n - 1)) in
         match parse expr with
         | Error e -> Printf.printf "Parse error: %s\n" e
         | Ok c ->
           let report =
             Greeks.compute_report ~market ~contract:c ~underlying
           in
           print_endline (Print.greeks_report (Print.smart_contract c) report)
       end

     | ":quit" | ":exit" -> exit 0

     | _ ->
       (* Fall through: try to parse as a contract expression *)
       (match parse line with
        | Error e ->
          Printf.printf "Unknown command or parse error: %s\n" e
        | Ok c ->
          Printf.printf "Contract : %s\n" (Print.smart_contract c);
          let known = Hashtbl.create (module String) in
          (match Checker.check ~known_underlyings:known c with
           | Ok ct ->
             Printf.printf "Check    : \xe2\x9c\x93 %s\n"
               (Sexp.to_string (Checker.sexp_of_currency_type ct))
           | Error errs ->
             Printf.printf "Check    : \xe2\x9c\x97\n";
             List.iter errs ~f:(fun e ->
               print_endline (Print.check_error e)))))

(* ──────────────────────────────── REPL loop ── *)

let run () =
  let market = Market.create ~valuation_date:(Skew_date.today ()) in
  Market.set_rate market Skew_currency.USD 0.05;
  print_endline "Skew \xe2\x80\x94 Financial Contract DSL";
  print_endline "Type :help for commands.\n";
  let rec loop () =
    print_string "skew> ";
    Out_channel.flush stdout;
    match In_channel.input_line In_channel.stdin with
    | None -> ()
    | Some line ->
      let line = String.strip line in
      if String.is_empty line then loop ()
      else begin
        (try dispatch market line
         with e ->
           Printf.printf "Error: %s\n" (Exn.to_string e));
        loop ()
      end
  in
  loop ()

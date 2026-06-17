module Skew_date = Date
open Core

(* Extract Const value from a float observable if it is one *)
let obs_const_value : float Observable.t -> float option = function
  | Observable.Const x -> Some x
  | _ -> None

let rec step = function
  (* Double negation *)
  | Contract.Give (Contract.Give c) -> step c
  (* And with Zero *)
  | Contract.And (Contract.Zero, c) -> step c
  | Contract.And (c, Contract.Zero) -> step c
  (* Scale by 0 *)
  | Contract.Scale (obs, _) when Option.equal Float.equal (obs_const_value obs) (Some 0.0) ->
    Contract.Zero
  (* Scale by 1 *)
  | Contract.Scale (obs, c) when Option.equal Float.equal (obs_const_value obs) (Some 1.0) ->
    step c
  (* Scale of Zero *)
  | Contract.Scale (_, Contract.Zero) -> Contract.Zero
  (* Give Zero *)
  | Contract.Give Contract.Zero -> Contract.Zero
  (* Truncate Zero *)
  | Contract.Truncate (_, Contract.Zero) -> Contract.Zero
  (* Then Zero *)
  | Contract.Then (Contract.Zero, c) -> step c
  (* Get Zero *)
  | Contract.Get Contract.Zero -> Contract.Zero
  (* Anytime Zero *)
  | Contract.Anytime Contract.Zero -> Contract.Zero
  (* Constant folding for nested scales *)
  | Contract.Scale (Observable.Const a, Contract.Scale (Observable.Const b, c)) ->
    step (Contract.Scale (Observable.Const (a *. b), c))
  (* Right-associate And *)
  | Contract.And (Contract.And (a, b), c) ->
    step (Contract.And (a, Contract.And (b, c)))
  (* Recurse into sub-contracts *)
  | Contract.Zero -> Contract.Zero
  | Contract.One _ as c -> c
  | Contract.Give c -> Contract.Give (step c)
  | Contract.And (c1, c2) -> Contract.And (step c1, step c2)
  | Contract.Or (c1, c2) -> Contract.Or (step c1, step c2)
  | Contract.Truncate (d, c) -> Contract.Truncate (d, step c)
  | Contract.Then (c1, c2) -> Contract.Then (step c1, step c2)
  | Contract.Scale (obs, c) -> Contract.Scale (obs, step c)
  | Contract.Get c -> Contract.Get (step c)
  | Contract.Anytime c -> Contract.Anytime (step c)

let simplify contract =
  let rec loop c =
    let c' = step c in
    if Contract.equal c c' then c else loop c'
  in
  loop contract

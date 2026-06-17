# Skew

A typed DSL for compositional financial contracts in OCaml, with a Monte Carlo pricer,
binomial lattice pricer, forward-mode automatic differentiation for Greeks, a contract
currency type checker, and a normalization rewrite system.

Inspired by Peyton Jones & Eber (2000), [Composing Contracts: An Adventure in Financial
Engineering](https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/).

[Explore →](https://skew.aryasomu.com) · [Source](https://github.com/arpjw/skew)

---

## Contract Language

Contracts are expressions in a combinator algebra. The combinator `european_call` is not
a primitive — it is defined entirely in terms of `Truncate`, `Get`, `Or`, `Scale`, and `One`:

```ocaml
let european_call underlying ccy strike expiry =
  Truncate (expiry,
    Get (
      Or (
        Scale (Observable.(spot underlying -. konst strike),
               One ccy),
        Zero)))
```

The pricing engine has no knowledge of "call options" — it only knows how to evaluate
`Or`, `Scale`, `Get`. The contract language is a data structure; the pricer is an
interpreter.

## Derived Contracts

| Contract | REPL syntax |
|---|---|
| European call | `call AAPL 150.0 2026-12-19` |
| European put | `put AAPL 150.0 2026-12-19` |
| American put | `aput AAPL 150.0 2026-12-19` |
| Zero-coupon bond | `zcb USD 2027-01-01 100.0` |
| Forward | `forward AAPL 150.0 2026-12-19` |

## Design

### Typed Observables via GADTs

The observable language is indexed by its value type:

```ocaml
type _ t =
  | Const    : 'a -> 'a t
  | Lift2    : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t
  | Spot     : string -> float t
  | Rate     : Currency.t -> float t
  | Greater  : float t * float t -> bool t
  | If       : bool t * 'a t * 'a t -> 'a t
  (* ... *)
```

`Scale` accepts only `float Observable.t` — passing a `bool` observable is a compile-time
type error. This is the same technique used in Jane Street's `typed_fields` and
`Incremental` libraries.

### Forward-Mode Automatic Differentiation

Delta is computed via pathwise dual differentiation, not finite differencing. A dual
number carries a primal and derivative component:

```ocaml
type t = { v: float; d: float }
```

Arithmetic is overloaded to propagate derivatives (`d/dx (f*g) = f'g + fg'`). To
compute delta with respect to spot, set `d = 1.0` on the spot observable and evaluate
the pricing function once — the `d` component of the output is delta. One pricing pass
yields the exact pathwise derivative.

Vega, theta, and rho use bump-and-reval (central finite difference) because those
parameters do not appear inside observable expressions and cannot be lifted to duals.

### Contract Currency Type Checker

The checker infers the currency type of every sub-contract and rejects mismatches
before pricing reaches the simulator:

```
skew> :check and (one USD) (one EUR)
Error: CurrencyMismatch in And:
  Left  : Single(USD)
  Right : Single(EUR)
  Contracts with different currencies cannot be combined without an FX leg.
```

Same architecture as Hindley-Milner type inference — structural recursion, unification
at combination points (`And`, `Or`, `Then`), propagation through wrappers (`Give`,
`Scale`, `Get`).

## REPL

```
skew> :set spot AAPL 155.0
Market: AAPL spot = 155.0000

skew> :set vol AAPL 0.25
Market: AAPL vol = 0.2500

skew> :set rate USD 0.05
Market: USD rate = 5.00%

skew> :price call AAPL 150.0 2026-12-19
Price    :  14.2300  (±0.0900, N=10000)

skew> :lattice call AAPL 150.0 2026-12-19
Price    :  14.3100  (N=500 steps)

skew> :greeks call AAPL 150.0 2026-12-19 AAPL
Contract : EuropeanCall(AAPL, USD, 150.00, 2026-12-19)
──────────────────────────────────────────────────────
Price    :  14.2300  (±0.0900)
Delta    :   0.6410
Vega     :   0.4120  (per 1 vol pt)
Theta    :  -0.0210  (per day)
Rho      :   0.0060  (per 1bp)

skew> :check and (one USD) (one EUR)
CurrencyMismatch in And:
  Left  : Single(USD)
  Right : Single(EUR)
  Contracts with different currencies cannot be combined without an FX leg.

skew> :simplify give (give (one USD))
Before : Give(Give(One(USD)))
After  : One(USD)

skew> :both call AAPL 150.0 2026-12-19
MC     :  14.2300  (±0.0900)
Lattice:  14.3100
Diff   :   0.0800  (0.56%)
```

## Architecture

```
currency ──► date ──► observable ──► contract ──► checker
                                              └──► simplify
market ──► dual ──► path ──► lattice ──► pricer ──► greeks
                                    └──► pricer ──► print ──► repl
```

## Known Limitations

1. **Correlation**: Multi-asset simulation uses independent GBM (ρ=0).
2. **Longstaff-Schwartz**: American options on MC paths use simplified exercise; use the lattice backend for American options.
3. **Smile**: Vol surface supports interpolation but not stochastic vol (Heston, SABR).
4. **Dividends**: Not modeled.
5. **Pathwise delta on discontinuous payoffs**: Biased for binary options and barriers.

## Build

```bash
opam install . --deps-only
dune build
dune exec test/test_suite.exe   # 68 tests
dune exec bin/main.exe          # REPL
```

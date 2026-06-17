# Skew

A typed DSL for compositional financial contracts in OCaml. Contracts are expressed as
algebraic combinations of primitives — `Zero`, `One`, `Give`, `And`, `Or`, `Scale`,
`Get`, `Truncate`, `Anytime` — and priced by two independent backends: a Monte Carlo
simulator and a Cox-Ross-Rubinstein binomial lattice. Greeks are computed via forward-mode
automatic differentiation implemented from scratch using dual numbers, without finite
differencing.

Inspired by Peyton Jones & Eber (2000),
[Composing Contracts: An Adventure in Financial Engineering](https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/).
This is not a port — it is a from-scratch OCaml implementation that extends the original
combinators with a GADT observable language, a currency type system, forward-mode AD for
Greeks, and a normalization rewrite system.

[Playground →](https://skew.aryasomu.com) · [Source](https://github.com/arpjw/skew)

---

## Contract Language

The pricing engine knows nothing about "call options." It only knows how to evaluate nine
primitive combinators. `european_call` is not a built-in — it is a definition:

```ocaml
let european_call underlying ccy strike expiry =
  Truncate (expiry,
    Get (
      Or (
        Scale (Observable.(spot underlying -. konst strike), One ccy),
        Zero)))
```

A call is: at most until `expiry` (`Truncate`), acquire (`Get`) the better of
(`Or`) receiving `S − K` units of currency or nothing (`Zero`). The pricer never sees
"call" — it sees `Or`, `Scale`, `Get`. This compositionality is the point: spreads,
collars, barriers, swaps, and structured products all reduce to the same nine forms.

### Combinator Reference

| Combinator | Meaning |
|---|---|
| `Zero` | No cashflows. Additive identity. |
| `One ccy` | Receive 1 unit of `ccy` now. |
| `Give c` | Flip all cashflows of `c`. |
| `And (c1, c2)` | Hold both contracts. Cashflows are additive. |
| `Or (c1, c2)` | Holder chooses the better contract at acquisition. |
| `Scale (obs, c)` | Multiply all cashflows of `c` by the observable `obs`. |
| `Truncate (t, c)` | `c` expires worthless after date `t`. |
| `Then (c1, c2)` | Activate `c2` when `c1` expires. |
| `Get c` | Acquire `c` at the first date it has non-negative value. |
| `Anytime c` | Like `Get` but holder chooses when. American semantics. |

### Derived Contracts

| Contract | Definition | REPL syntax |
|---|---|---|
| European call | `Truncate(T, Get(Or(Scale(S−K, One ccy), Zero)))` | `call AAPL 150.0 2026-12-19` |
| European put | `Truncate(T, Get(Or(Scale(K−S, One ccy), Zero)))` | `put AAPL 150.0 2026-12-19` |
| American call | `Truncate(T, Anytime(Or(Scale(S−K, One ccy), Zero)))` | `acall AAPL 150.0 2026-12-19` |
| American put | `Truncate(T, Anytime(Or(Scale(K−S, One ccy), Zero)))` | `aput AAPL 150.0 2026-12-19` |
| Forward | `Truncate(T, Get(And(Scale(S, One ccy), Give(Scale(K, One ccy)))))` | `forward AAPL 150.0 2026-12-19` |
| Zero-coupon bond | `Scale(N, Truncate(T, Get(One ccy)))` | `zcb USD 2027-01-01 100.0` |

---

## Design

### Typed Observables via GADTs

The observable language is parameterized by its value type:

```ocaml
type _ t =
  | Const    : 'a -> 'a t
  | Lift1    : ('a -> 'b) * 'a t -> 'b t
  | Lift2    : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t
  | Spot     : string -> float t
  | Rate     : Currency.t -> float t
  | Greater  : float t * float t -> bool t
  | Equal    : float t * float t -> bool t
  | If       : bool t * 'a t * 'a t -> 'a t
  | Date     : Date.t t
  | Horizon  : Date.t -> float t
```

`Scale` accepts `float Observable.t` — the type parameter enforces this at compile time.
Passing a `bool` observable to `Scale` is a type error the compiler catches before the
code runs. This is the same technique used in Jane Street's `Incremental` and
`typed_fields`.

Evaluation is a single structural recursion over the GADT. No runtime type checks. No
`Obj.magic`. The type system does the work.

### Contract Simplification

A rewrite system normalizes contracts before pricing. Rules are applied bottom-up in a
single post-order pass, repeated to fixed point:

| Rule | Reduction |
|---|---|
| `Give (Give c)` | `c` |
| `And (Zero, c)` | `c` |
| `And (c, Zero)` | `c` |
| `Scale (0, _)` | `Zero` |
| `Scale (1, c)` | `c` |
| `Scale (_, Zero)` | `Zero` |
| `Give Zero` | `Zero` |
| `Truncate (_, Zero)` | `Zero` |
| `Then (Zero, c)` | `c` |
| `Get Zero` | `Zero` |
| `Anytime Zero` | `Zero` |
| `Scale (a, Scale (b, c))` | `Scale (a*b, c)` — constant folding |
| `And (And (a, b), c)` | `And (a, And (b, c))` — right-associate |

Termination is guaranteed because every rule either reduces AST size or holds it
constant. The fixed-point loop runs until a full pass produces no rewrites.

```
skew> :simplify scale 1.0 (give (give (and zero (one USD))))
Before : Scale(1.0, Give(Give(And(Zero, One(USD)))))
After  : One(USD)
```

### Currency Type Checker

Before any contract reaches the pricer, a type checker infers and unifies currency types
across the AST. The rules mirror Hindley-Milner: propagate through wrappers (`Give`,
`Scale`, `Get`, `Anytime`, `Truncate`), unify at combination points (`And`, `Or`,
`Then`). `Void` (from `Zero`) is compatible with any currency; two distinct non-void
currencies in the same `And` or `Or` is an error.

```
skew> :check and (one USD) (one EUR)
Error: CurrencyMismatch in And:
  Left  : Single(USD)
  Right : Single(EUR)
  Contracts with different currencies cannot be combined without an FX leg.
```

The pricer refuses to run on contracts that fail the check.

### Dual-Number Forward-Mode AD

Greeks are computed by forward-mode automatic differentiation, not finite differencing.
A dual number carries a primal and a directional derivative:

```ocaml
type t = { v: float; d: float }

let ( *. ) a b = { v = a.v *. b.v; d = a.d *. b.v +. a.v *. b.d }
let exp a = let e = Float.exp a.v in { v = e; d = a.d *. e }
let sqrt a = let s = Float.sqrt a.v in { v = s; d = a.d /. (2.0 *. s) }
```

To compute delta: set `d = 1.0` on the spot observable and evaluate the pricing function
once. The `d` component of the output is the exact pathwise derivative — not an
approximation. The dual arithmetic automatically propagates `∂/∂S` through every
arithmetic operation in the payoff.

Vega, theta, and rho use central finite difference (bump-and-reval) because those
parameters enter through the path simulation, not through observable expressions, and
cannot be cleanly lifted to dual numbers.

The dual library is implemented from scratch: addition, subtraction, multiplication,
division, `exp`, `log`, `sqrt`, `pow`, `max`, `min`, and `norm_cdf` (for
Black-Scholes closed-form verification), all overloaded to propagate derivatives.

### Two Pricing Backends behind One Interface

Both backends satisfy the same module type:

```ocaml
module type PRICER = sig
  type config
  val default_config : config
  val price           : config:config -> market:Market.t -> contract:Contract.t -> float
  val price_with_stderr : config:config -> market:Market.t -> contract:Contract.t -> float * float
end
```

**Monte Carlo** (`MonteCarlo : PRICER`): simulates GBM paths using the Box-Muller
transform for standard normals and exact log-normal steps between observation dates:

```
S(t+dt) = S(t) · exp((r − ½σ²)dt + σ√dt · Z),  Z ~ N(0,1)
```

Default: 10,000 paths, 252 steps. Returns mean and standard error. Handles arbitrary
contract trees by structural recursion.

**Lattice** (`LatticePricer : PRICER`): Cox-Ross-Rubinstein binomial tree with:

```
u = exp(σ√dt),   d = 1/u,   p_u = (exp(r·dt) − d) / (u − d)
```

Forward pass fills the price tree. Backward induction discounts expected values,
with an early-exercise check at each node for American options (`Anytime`):

```
V(i,j) = max(payoff(S(i,j)), e^{−r·dt} · (p_u · V(i+1,j+1) + p_d · V(i+1,j)))
```

Default: 500 steps. Recognizes standard contract forms (European/American call/put,
`Give`, `Scale`) and falls back to Monte Carlo for unsupported trees with a warning.

### Market Data

`Market.t` holds spot prices, flat or surface-interpolated vols, and risk-free rates per
currency. Vol surface interpolation is bilinear: find the bracketing strike and expiry
indices, interpolate in the strike direction at each expiry, then interpolate the results
in the expiry direction. Values outside the grid clamp to the boundary.

---

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

skew> :both call AAPL 150.0 2026-12-19
MC     :  14.2300  (±0.0900)
Lattice:  14.3100
Diff   :   0.0800  (0.56%)

skew> :greeks call AAPL 150.0 2026-12-19 AAPL
Contract : EuropeanCall(AAPL, USD, 150.00, 2026-12-19)
──────────────────────────────────────────────────────
Price    :  14.2300  (±0.0900)
Delta    :   0.6410
Vega     :   0.4120  (per 1 vol pt)
Theta    :  -0.0210  (per day)
Rho      :   0.0060  (per 1bp)

skew> :check and (one USD) (one EUR)
Error: CurrencyMismatch in And:
  Left  : Single(USD)
  Right : Single(EUR)

skew> :simplify give (give (scale 1.0 (and zero (one USD))))
Before : Give(Give(Scale(1.0, And(Zero, One(USD)))))
After  : One(USD)
```

Commands: `:price`, `:lattice`, `:both`, `:greeks`, `:check`, `:simplify`,
`:set spot/vol/rate`, `:show market`, `:show contract`, `:help`, `:quit`.

---

## Browser Playground

The same OCaml library compiles to JavaScript via
[js_of_ocaml](https://ocsigen.org/js_of_ocaml/). The entire pricing engine — GBM
simulation, lattice, dual-number Greeks — runs in the browser with no server.

To keep the UI responsive, all computation runs in a
[Web Worker](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API). The
main thread dispatches a request and updates the DOM when results arrive; it is never
blocked. A request queue ensures rapid typing replaces stale pending work rather than
stacking it.

The web build uses 1,000 paths / 52 steps (vs 10,000 / 252 for the native REPL) for
interactive latency. The native binary and the browser bundle share the same `skew_lib`
— the only platform-specific code is in `bin/main.ml` and `web/main_js.ml`.

---

## Test Suite

68 tests across 11 modules, run with Alcotest. Selected numerical benchmarks:

| Test | What it checks |
|---|---|
| MC European call | Within $0.50 of Black-Scholes at N=50,000 |
| Lattice European call | Within $0.05 of BS at N=500 steps |
| Lattice European put | Within $0.05 of BS at N=500 steps |
| Put-call parity (MC) | `C − P = S − K·e^{−rT}` within $0.20 |
| Put-call parity (lattice) | Same identity within $0.01 |
| MC vs lattice | Agree within $0.30 on same contract |
| American put ≥ European put | Early exercise premium is non-negative |
| Delta (MC vs BS) | Within 0.02 of analytical delta |
| Zero-coupon bond | Within $0.01 of `N·e^{−rT}` |
| Simplification fixed-point | All rewrite rules verified individually |
| Currency checker | Mismatch detection for all combinator forms |

---

## Architecture

```
currency ──► date ──► observable ──► contract ──► checker
                                              └──► simplify
market ──► dual ──► path ──► pricer (MonteCarlo)  ──► greeks
                └──► lattice ──► pricer (Lattice)  └──► print ──► repl
```

---

## Known Limitations

1. **Correlation**: Multi-asset simulation uses independent GBM paths (ρ = 0). Correlated
   simulation via Cholesky decomposition is not implemented.

2. **Longstaff-Schwartz**: American options on Monte Carlo paths use simplified exercise.
   Use the lattice backend for American options.

3. **Smile**: The vol surface supports bilinear interpolation but not stochastic vol
   (Heston, SABR). Vols are flat unless the surface is explicitly populated.

4. **Dividends**: Discrete and continuous dividends are not modeled.

5. **Pathwise delta on discontinuous payoffs**: Binary options, barriers, and contracts
   with kinks produce biased pathwise deltas. Use bump-and-reval for those.

---

## Build

```bash
opam install . --deps-only
dune build
dune exec test/test_suite.exe   # 68 tests
dune exec bin/main.exe          # REPL
```

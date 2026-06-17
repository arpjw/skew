importScripts('./skew.js');

// Default spot/vol for common underlyings. Applied before every run so that
// contracts using these tickers work without the user manually entering values.
// The active request's spot/vol always overrides for the current underlying.
var DEFAULT_SEEDS = [
  { u: 'AAPL', spot: 195.0,  vol: 0.28 },
  { u: 'NVDA', spot: 131.0,  vol: 0.45 },
  { u: 'SPX',  spot: 5850.0, vol: 0.16 },
];

onmessage = function(e) {
  var d = e.data;
  try {
    if (typeof Skew === 'undefined') throw new Error('Skew runtime not loaded');

    // Seed defaults, then override with the user-supplied values for the
    // current underlying so the input fields always take precedence.
    DEFAULT_SEEDS.forEach(function(s) {
      Skew.setSpot(s.u, s.spot);
      Skew.setVol(s.u, s.vol);
    });
    Skew.setSpot(d.underlying, d.spot);
    Skew.setVol(d.underlying, d.vol);
    Skew.setRate('USD', d.rate);

    var check      = Skew.check(d.expr);
    var simplified = Skew.simplify(d.expr);
    var price      = Skew.price(d.expr);
    var lattice    = Skew.lattice(d.expr);
    var greeks     = Skew.greeks(d.expr, d.underlying);

    postMessage({ ok: true, id: d.id, check: check, simplified: simplified,
                  price: price, lattice: lattice, greeks: greeks });
  } catch(err) {
    postMessage({ ok: false, id: d.id, error: err.toString() });
  }
};

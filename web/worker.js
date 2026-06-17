importScripts('./skew.js');

var workerReady = typeof Skew !== 'undefined';

onmessage = function(e) {
  var d = e.data;
  try {
    if (!workerReady || typeof Skew === 'undefined') {
      throw new Error('Skew runtime not loaded');
    }
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

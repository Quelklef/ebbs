import * as Rational from './Rational.js';

export type T = Bound;

type Bound = '+inf' | '-inf' | Rational.T;

export function le(x : Bound, y : Bound) : boolean {
  return x === '-inf' || y === '+inf' || (x !== '+inf' && y !== '-inf' && Rational.le(x, y));
}

export function nonfinite(x : Bound) : x is '+inf' | '-inf' {
  return typeof x !== 'object';
}

export function finite(x : Bound) : x is Rational.T {
  return typeof x !== 'string';
}

export function add(x : Bound, y : Bound) : Bound {
  if (nonfinite(x) || nonfinite(y)) {
    if (x === y) return x;
    else throw Error('Cannot add +inf to -inf');
  }
  return Rational.add(x, y);
}

export function sub(x : Bound, y : Bound) : Bound {
  return add(x, neg(y));
}

export function neg(x : Bound) : Bound {
  return x === '+inf' ? '-inf' : x === '-inf' ? '+inf' : Rational.neg(x);
}

export function mul(x : Bound, y : Bound) : Bound {
  if (    nonfinite(x) && finite(y) && Rational.eq(y, Rational.zero)
       || nonfinite(y) && finite(x) && Rational.eq(x, Rational.zero))
     throw Error('Cannot multiply nonfinite bound by zero!');
  return (
    nonfinite(x) || nonfinite(y)
      ? setSgn('+inf', (sgn(x) * sgn(y)) as (-1 | 1))
      : Rational.mul(x, y)
  );
}

export function div(x : Bound, y : Bound) : Bound {
  return mul(x, rec(y));
}

export function rec(x : Bound) : Bound {
  if (nonfinite(x)) return Rational.zero;
  if (Rational.eq(x, Rational.zero)) return '+inf';
  return Rational.rec(x);
}

export function sgn(x : Bound): -1 | 1 {
  return (
    x === '-inf' ? -1
      : x === '+inf' ? 1
      : Rational.sgn(x)
  );
}

export function setSgn(x : Bound, sgn : -1 | 1) {
  return (
    nonfinite(x)
      ? sgn === -1 ? '-inf' : '+inf'
      : Rational.setSgn(x, sgn)
  );
}

export function pretty(x : Bound) : string {
  if (x === '+inf') return '∞';
  if (x === '-inf') return '-∞';
  return Rational.toDecimal(x);
}

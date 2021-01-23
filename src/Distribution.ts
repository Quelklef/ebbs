import * as Rational from './Rational.js';
import * as Bound from './Bound.js';

export type T = Distribution;

type Distribution = DiracDelta | Piecewise | Polynomial | Offset | Dilated;

interface DiracDelta
  { type : 'DiracDelta'
  , x : Rational.T
  }

interface Piecewise
  { type : 'Piecewise'
  , boundaries : Array<Rational.T>  // n boundaries
  , components : Array<Distribution>  // n+1 component distributions
  }

interface Polynomial
  { type : 'Polynomial'
  , coefficients : Array<Rational.T>  // highest exponent first
  }

interface Offset
  { type : 'Offset'
  , base : Distribution
  , offset : Rational.T
  }

interface Dilated
  { type : 'Dilated'
  , base : Distribution
  , xDilation : Rational.T
  , yDilation : Rational.T
  }

export function valueAt(distn : Distribution, x : Bound.T) : Bound.T {
  switch (distn.type) {
    case 'DiracDelta':
      return Rational.zero;
    case 'Piecewise':
      return valueAt(distn.components[distn.boundaries.filter(b => b < x).length], x);
    case 'Polynomial':
      return (
        x === '+inf' || x === '-inf'
          ? Bound.mul('+inf', distn.coefficients[0] ?? Rational.zero)
          : [...distn.coefficients].reduce((acc, coeff) => Rational.add(Rational.mul(acc, x), coeff), Rational.zero)
      );
    case 'Offset':
      return valueAt(distn.base, Bound.sub(x, distn.offset));
    case 'Dilated':
      return Bound.mul(distn.yDilation, valueAt(distn.base, Bound.div(x, distn.xDilation)));
  }
}

export function integrate(distn : Distribution, x0 : Bound.T, xf : Bound.T) : Bound.T {
  switch (distn.type) {
    case 'DiracDelta':
      return Bound.le(x0, distn.x) && Bound.le(distn.x, xf) ? Rational.one : Rational.zero;
    case 'Piecewise':
      let result : Bound.T = Rational.zero;
      for (let i = 0; i < distn.components.length; i++) {
        const left : Bound.T = distn.boundaries[i] ?? '-inf';
        const right : Bound.T = distn.boundaries[i + 1] ?? '+inf';
        const component = distn.components[i];
        result = Bound.add(result, integrate(component, left, right));
      }
      return result;
    case 'Polynomial':
      const antidifferentiated: Polynomial =
        { type : 'Polynomial'
        , coefficients : [Rational.zero].concat(distn.coefficients.map((coeff, i) => Rational.div(coeff, Rational.from(i))))
        };
      return Bound.sub(valueAt(antidifferentiated, xf), valueAt(antidifferentiated, x0));
    case 'Offset':
      return integrate(distn.base, Bound.sub(x0, distn.offset), Bound.sub(xf, distn.offset));
    case 'Dilated':
      return Bound.mul(distn.yDilation, integrate(distn.base, Bound.div(x0, distn.xDilation), Bound.div(xf, distn.xDilation)));
  }
}

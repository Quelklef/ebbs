import * as Model from './Model.js';
import * as Distribution from './Distribution.js';
import * as Bound from './Bound.js';
import * as Rational from './Rational.js';
import * as Identifier from './Identifier.js';
import * as Pool from './Pool.js';
import * as Strymbol from './Strymbol.js';
import * as Maybe from './Maybe.js';

export type T = Movement

interface Movement
  { type : 'Movement'
  , identifier : Identifier.T
  , source : Identifier.T
  , target : Identifier.T
  , principle : Rational.T
  , distribution : Distribution.T
  , time : Rational.T
  , threshold : Maybe.T<bigint>  // TODO: SEMANTICS
  , tags : Array<Strymbol.T>
  , description: string
  }

// For a given time interval, how much did a given movement contribute to a given pool?
export function given(movement : Movement, pool : Pool.T, t0 : Bound.T, tf : Bound.T) : Bound.T {
  const sgn =
    Identifier.eq(movement.target, pool.identifier) ? Rational.one
    : Identifier.eq(movement.source, pool.identifier) ? Rational.neg(Rational.one)
    : Rational.zero;
  return Bound.mul(sgn, moved(movement, t0, tf));
}

// For a given time interval, how much did a given movement detract from a given pool?
export function taken(movement : Movement, pool : Pool.T, t0 : Bound.T, tf : Bound.T) : Bound.T {
  return Bound.neg(given(movement, pool, t0, tf));
}

// For a given time interval, how much value did a movement move around, in total?
export function moved(movement : Movement, t0 : Bound.T, tf : Bound.T) : Bound.T {
  const shifted : Distribution.T = (
    { type : 'Offset'
    , base : movement.distribution
    , offset : movement.time
    });
  const dilated : Distribution.T = (
    { type : 'Dilated'
    , base : shifted
    , xDilation : Rational.one
    , yDilation : movement.principle
    });
  return Distribution.integrate(dilated, t0, tf);
}

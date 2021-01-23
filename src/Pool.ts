import * as Bound from './Bound.js';
import * as Movement from './Movement.js';
import * as Model from './Model.js';
import * as Strymbol from './Strymbol.js';
import * as Identifier from './Identifier.js';
import * as Rational from './Rational.js';

export type T = Pool

interface Pool
  { type : 'Pool'
  , identifier : Identifier.T
  , name : string
  , tags : Array<Strymbol.T>
  , group : Strymbol.T
  , description: string
  }

// What is the value of a pool at a given time?
export function valueAt(model : Model.T, pool : Pool, time : Bound.T) : Bound.T {
  return sum(model.movements.map(movement => Movement.given(movement, pool, '-inf', time)));

  function sum(rationals : Array<Bound.T>) : Bound.T {
    return rationals.reduce((acc, x) => Bound.add(acc, x), Rational.zero);
  }
}

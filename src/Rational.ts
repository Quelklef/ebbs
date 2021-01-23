import * as Maybe from './Maybe.js';

export type T = Rational;

interface Rational
  { type : 'Rational'
  , sign : 1 | -1
  , numerator : bigint
  , denominator : bigint
  }

export const zero : Rational =
  { type : 'Rational'
  , sign : 1
  , numerator : 0n
  , denominator : 1n
  }

export const one : Rational =
  { type : 'Rational'
  , sign : 1
  , numerator : 1n
  , denominator : 1n
  }

export function from(n : bigint | number | Rational) : Rational {
  switch (typeof n) {
    case 'bigint':
      return (
        { type : 'Rational'
        , sign : n < 0n ? -1 : 1
        , numerator : n < 0n ? -n : n
        , denominator : 1n
        });
    case 'number':
      if (Math.round(n) !== n) throw Error('Cannot convert non-integral number to Rational');
      return (
        { type : 'Rational'
        , sign : n < 0 ? -1 : 1
        , numerator : BigInt(Math.abs(n))
        , denominator : 1n
        });
    case 'object':
      return n;
  }
}

export function parse(s : string) : Maybe.T<Rational> {
  s = s.trim();

  if (s.split('.').length === 2
   && !s.split('.')[1].includes('-')
   && s.split('.').map(part => parseInt(part, 10)).every(part => !Number.isNaN(part)))
  {
    const [signed_numerator, denominator] = s.split('.').map(part => BigInt(parseInt(part, 10)));
    const rational : Rational = (
      { type : 'Rational'
      , sign : signed_numerator < 0 ? -1 : 1
      , numerator : signed_numerator < 0n ? -signed_numerator : signed_numerator
      , denominator : denominator
      });
    return { type : 'Just', value : rational };
  }

  if (!Number.isNaN(parseInt(s, 10))) {
    return { type : 'Just', value : from(parseInt(s, 10)) };
  }

  return { type : 'Nothing' };
}

export function toDecimal(q : Rational) : string {
  // TODO
  return eval(`${q.sign} * ${q.numerator} / ${q.denominator}`) + '';
}

function simplify(q : Rational) : Rational {
  const factor = gcd(q.numerator, q.denominator);
  return (
    { type : 'Rational'
    , sign : q.sign
    , numerator : q.numerator / factor
    , denominator : q.denominator / factor
    });

  function gcd(x : bigint, y : bigint) : bigint {
    return y === 0n ? x : gcd(y, x % y);
  }
}

export function add(a : Rational, b : Rational) : Rational {
  const signed_numerator = BigInt(a.sign) * a.numerator * b.denominator + BigInt(b.sign) * b.numerator * a.denominator;
  const unsimplified : Rational =
    { type : 'Rational'
    , sign : signed_numerator < 0 ? -1 : 1
    , numerator : signed_numerator < 0 ? -signed_numerator : signed_numerator
    , denominator : a.denominator * b.denominator
    };
  return simplify(unsimplified);
}

export function sub(a : Rational, b : Rational) : Rational {
  return add(a, neg(b));
}

export function neg(a : Rational) : Rational {
  return { ...a, sign : -1 };
}

export function mul(a : Rational, b : Rational) : Rational {
  return (
    { type : 'Rational'
    , sign : (a.sign * b.sign) as (1 | -1)
    , numerator : a.numerator * b.numerator
    , denominator : a.denominator * b.denominator
    });
}

export function div(a : Rational, b : Rational) : Rational {
  return mul(a, rec(b));
}

export function rec(a : Rational): Rational {
  return { ...a, numerator : a.denominator, denominator : a.numerator };
}

export function eq(a : Rational, b : Rational) : boolean {
  return BigInt(a.sign) * a.numerator * b.denominator == BigInt(b.sign) * b.numerator * a.denominator;
}

export function ne(a : Rational, b : Rational) : boolean {
  return BigInt(a.sign) * a.numerator * b.denominator != BigInt(b.sign) * b.numerator * a.denominator;
}

export function lt(a : Rational, b : Rational) : boolean {
  return BigInt(a.sign) * a.numerator * b.denominator < BigInt(b.sign) * b.numerator * a.denominator;
}

export function le(a : Rational, b : Rational) : boolean {
  return BigInt(a.sign) * a.numerator * b.denominator <= BigInt(b.sign) * b.numerator * a.denominator;
}

export function gt(a : Rational, b : Rational) : boolean {
  return BigInt(a.sign) * a.numerator * b.denominator > BigInt(b.sign) * b.numerator * a.denominator;
}

export function ge(a : Rational, b : Rational) : boolean {
  return BigInt(a.sign) * a.numerator * b.denominator >= BigInt(b.sign) * b.numerator * a.denominator;
}

export function sgn(a : Rational) : -1 | 1 {
  return a.sign;
}

export function setSgn(a : Rational, sgn : -1 | 1) : Rational {
  return { ...a, sign : sgn };
}

export function abs(a : Rational) : Rational {
  return setSgn(a, 1);
}

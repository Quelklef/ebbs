import * as Rational from './Rational.js';

export type T = Duration

interface Duration
  { type : 'Duration'
  , seconds : Rational.T
  }

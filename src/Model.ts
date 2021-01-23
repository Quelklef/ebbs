import * as Movement from './Movement.js';
import * as Pool from './Pool.js';

export type T = Model

interface Model
  { type : 'Model'
  , pools : Array<Pool.T>
  , movements : Array<Movement.T>
  , identifierCount: bigint
  }

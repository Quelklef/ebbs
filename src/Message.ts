import * as Model from './Model.js';
import * as Identifier from './Identifier.js';
import * as Rational from './Rational.js';
import * as Movement from './Movement.js';
import * as Pool from './Pool.js';

export type T = Message;

type Message
  = CreatePool
  | CreateMovement
  | ModifyPool
  | ModifyMovement
  | Destroy

interface CreatePool
  { type : 'CreatePool'
  }

interface CreateMovement
  { type : 'CreateMovement'
  }

interface Destroy
  { type : 'Destroy'
  , target : Identifier.T
  }

type ModifyPool =
  { type : 'ModifyPool'
  , target : Identifier.T
  , delta : Partial<Pool.T>
  }

type ModifyMovement =
  { type : 'ModifyMovement'
  , target : Identifier.T
  , delta : Partial<Movement.T>
  }

export function update(model : Model.T, message : Message, now : bigint) : Model.T {
  switch (message.type) {

    case 'CreatePool':
      const pool : Pool.T =
        { type : 'Pool'
        , identifier : Identifier.from(model.identifierCount)
        , name : 'Unnamed Pool'
        , tags : []
        , group : { type : 'Sym', value : 'none' }
        , description : ''
        }
      return (
        { ...model
        , identifierCount : model.identifierCount + 1n
        , pools : model.pools.concat([pool])
        });

    case 'CreateMovement':
      if (model.pools.length === 0) return model;
      const movement : Movement.T =
        { type : 'Movement'
        , identifier : Identifier.from(model.identifierCount)
        , source : model.pools[Number( now     % BigInt(model.pools.length))].identifier
        , target : model.pools[Number((now+1n) % BigInt(model.pools.length))].identifier
        , principle : Rational.zero
        , time : Rational.from(now)
        , distribution : { type : 'DiracDelta' }
        , threshold : { type : 'Nothing' }
        , tags : []
        , description : ''
        };
      return (
        { ...model
        , identifierCount : model.identifierCount + 1n
        , movements : model.movements.concat([movement])
        });

    case 'ModifyPool':
      return (
        { ...model
        , pools : model.pools.map(
            pool => Identifier.eq(pool.identifier, message.target)
                   ? { ...pool, ...message.delta }
                   : pool )
        });

    case 'ModifyMovement':
      return (
        { ...model
        , movements : model.movements.map(
            movement => Identifier.eq(movement.identifier, message.target)
                   ? { ...movement, ...message.delta }
                   : movement )
        });

    case 'Destroy':
      return (
        { ...model
        , pools : model.pools.filter(pool => !Identifier.eq(pool.identifier, message.target))
        , movements : model.movements.filter(movement => !Identifier.eq(movement.identifier, message.target))
        });

  }
}

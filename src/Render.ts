import * as Message from './Message.js';
import * as Model from './Model.js';
import * as Rational from './Rational.js';
import * as Bound from './Bound.js';
import * as Movement from './Movement.js';
import * as Pool from './Pool.js';
import * as Identifier from './Identifier.js';

type Send = (message : Message.T) => void;

export function render(model : Model.T, send : Send, now : bigint) : HTMLElement {
  const $container = document.createElement('div');
  $container.append(
    renderPoolTitle(send),
    ...model.pools.map(pool => renderPool(model, pool, send, now)),
    renderAddPoolButton(send),
    renderMovementTitle(send),
    ...model.movements.map(movement => renderMovement(model, movement, send)),
    renderAddMovementButton(send),
  );
  return $container;
}

function renderPoolTitle(send : Send) : HTMLElement {
  const $title = document.createElement('h3');
  $title.innerText = 'Pools';
  return $title;
}

function renderPool(model : Model.T, pool : Pool.T, send : Send, now : bigint) : HTMLElement {
  const $name = document.createElement('input');
  $name.type = 'text';
  $name.value = pool.name;
  $name.addEventListener('blur', () =>
    send({ type : 'ModifyPool', target : pool.identifier, delta : { name : $name.value } })
  );

  const $value = document.createElement('span');
  $value.innerText = Bound.pretty(Pool.valueAt(model, pool, Rational.from(now)));
  // TODO: ^ update every second

  const $delete = document.createElement('button');
  $delete.innerText = 'delete';
  $delete.addEventListener('click', () => send({ type : 'Destroy', target : pool.identifier }));

  const $pool = document.createElement('div');
  $pool.append(`#${pool.identifier.value} | `, $name, $value, $delete);
  return $pool;
}

function renderAddPoolButton(send : Send) : HTMLElement {
  const $button = document.createElement('button');
  $button.innerText = 'new pool';
  $button.addEventListener('click', () => send({ type : 'CreatePool' }));
  const $container = document.createElement('p');
  $container.append($button);
  return $container;
}

function renderMovementTitle(send : Send) : HTMLElement {
  const $title = document.createElement('h3');
  $title.innerText = 'Movements';
  return $title;
}

function renderMovement(model : Model.T, movement : Movement.T, send : Send) : HTMLElement {
  const $source = makeEndpoint('source');
  const $target = makeEndpoint('target');

  const $principle = document.createElement('input');
  $principle.type = 'number';
  $principle.value = Rational.toDecimal(movement.principle);
  $principle.addEventListener('blur', () => {
    const maybePrinciple = Rational.parse($principle.value);
    if (maybePrinciple.type !== 'Just') return;
    send({ type : 'ModifyMovement', target : movement.identifier, delta : { principle: maybePrinciple.value } });
  });

  // TODO:
  // distribution : Distribution.T

  const $description = document.createElement('input');
  $description.type = 'text';
  $description.placeholder = 'description';
  $description.value = movement.description;
  $description.addEventListener('blur', () =>
    send({ type : 'ModifyMovement', target : movement.identifier, delta : { description : $description.value } })
  );

  const $delete = document.createElement('button');
  $delete.innerText = 'delete';
  $delete.addEventListener('click', () => send({ type : 'Destroy', target : movement.identifier }));

  const $movement = document.createElement('div');
  $movement.append(
    `#${movement.identifier.value} | `,
    makeLabel($source, 'From:'),
    makeLabel($target, 'To:'),
    makeLabel($principle, 'Amount:'),
    $description,
    $delete,
  );
  return $movement;

  function makeEndpoint(key : string) : HTMLElement {
    const $endpoint = document.createElement('select');
    $endpoint.append(...model.pools.map(pool => {
      const $option = document.createElement('option');
      $option.innerText = pool.name;
      $option.value = pool.identifier.value;
      $option.selected = Identifier.eq(pool.identifier, (movement as any)[key]);
      return $option;
    }));
    $endpoint.addEventListener('change', () => {
      const identifier = { type : 'Identifier', value : $endpoint.value };
      send({ type : 'ModifyMovement', target : movement.identifier, delta : { [key] : identifier } })
    });
    return $endpoint;
  }

  function makeLabel($el : HTMLElement, label : string) : HTMLElement {
    const $container = document.createElement('span');
    $container.append(label, $el);
    return $container;
  }
}

function renderAddMovementButton(send : Send) : HTMLElement {
  const $button = document.createElement('button');
  $button.innerText = 'new movement';
  $button.addEventListener('click', () => send({ type : 'CreateMovement' }));
  const $container = document.createElement('p');
  $container.append($button);
  return $container;
}

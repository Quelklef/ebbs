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
  const $title = document.createElement('h2');
  $title.innerText = 'Pools';
  return $title;
}

function renderPool(model : Model.T, pool : Pool.T, send : Send, now : bigint) : HTMLElement {
  const $name = document.createElement('input');
  $name.classList.add('--show-n-edit', ':pool:name');
  $name.type = 'text';
  $name.value = pool.name;
  $name.addEventListener('blur', () =>
    send({ type : 'ModifyPool', target : pool.identifier, delta : { name : $name.value } })
  );

  const $value = document.createElement('span');
  $value.classList.add(':pool:value');
  $value.innerText = Bound.pretty(Pool.valueAt(model, pool, Rational.from(now)));
  // TODO: ^ update every second

  const $delete = document.createElement('button');
  $delete.classList.add('delete-button', ':pool:delete');
  $delete.innerText = 'delete_outline';
  $delete.addEventListener('click', () => send({ type : 'Destroy', target : pool.identifier }));

  const $pool = document.createElement('div');
  $pool.classList.add(':pool');
  $pool.append($name, $value, $delete);
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
  const $title = document.createElement('h2');
  $title.innerText = 'Movements';
  return $title;
}

function renderMovement(model : Model.T, movement : Movement.T, send : Send) : HTMLElement {
  const $source = makeEndpoint('source');
  const $target = makeEndpoint('target');

  const $description = document.createElement('input');
  $description.classList.add('--show-n-edit', ':movement:description');
  $description.type = 'text';
  $description.placeholder = 'description';
  $description.value = movement.description;
  $description.addEventListener('blur', () =>
    send({ type : 'ModifyMovement', target : movement.identifier, delta : { description : $description.value } })
  );

  const $principleData = document.createElement('input');
  $principleData.classList.add('--show-n-edit', ':movement:principle:data');
  $principleData.type = 'number';
  $principleData.value = Rational.toDecimal(movement.principle);
  $principleData.addEventListener('blur', () => {
    const maybePrinciple = Rational.parse($principleData.value);
    if (maybePrinciple.type !== 'Just') return;
    send({ type : 'ModifyMovement', target : movement.identifier, delta : { principle : maybePrinciple.value } });
  });
  const $principle = document.createElement('span');
  $principle.classList.add(':movement:principle');
  $principle.append('Amount: ', $principleData);

  const $timeData = document.createElement('input');
  $timeData.classList.add('--show-n-edit', ':movement:time:data');
  $timeData.type = 'number';
  $timeData.value = Rational.toDecimal(movement.time);
  $timeData.addEventListener('blur', () => {
    const maybeTime = Rational.parse($timeData.value);
    if (maybeTime.type !== 'Just') return;
    send({ type : 'ModifyMovement', target : movement.identifier, delta : { time : maybeTime.value } });
  });
  const $time = document.createElement('span');
  $time.classList.add(':movement:time');
  $time.append('Time: ', $timeData);

  const $distributionData = document.createElement('input');
  $distributionData.classList.add('--show-n-edit', ':movement:distribution:data');
  $distributionData.type = 'text';
  $distributionData.value = '<todo>';
  const $distribution = document.createElement('span');
  $distribution.classList.add(':movement:distribution');
  $distribution.append('Distribution: ', $distributionData);

  const $delete = document.createElement('button');
  $delete.classList.add('delete-button', ':movement:delete');
  $delete.innerText = 'delete_outline';
  $delete.addEventListener('click', () => send({ type : 'Destroy', target : movement.identifier }));

  const $rarr = document.createElement('span');
  $rarr.innerText = '→';

  const $controls = document.createElement('div');
  $controls.classList.add(':movement:controls');
  $controls.append($source, $rarr, $target, $principle, $time, $distribution, $delete);

  const $movement = document.createElement('div');
  $movement.classList.add(':movement');
  $movement.append($description, $controls);
  return $movement;

  function makeEndpoint(key : string) : HTMLElement {
    const $endpoint = document.createElement('select');
    $endpoint.classList.add('--show-n-edit', ':movement:endpoint');
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
}

function renderAddMovementButton(send : Send) : HTMLElement {
  const $button = document.createElement('button');
  $button.innerText = 'new movement';
  $button.addEventListener('click', () => send({ type : 'CreateMovement' }));
  const $container = document.createElement('p');
  $container.append($button);
  return $container;
}

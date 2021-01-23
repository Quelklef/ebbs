import * as Message from './Message.js';
import * as Render from './Render.js';
import * as Model from './Model.js';
import { loadJson, dumpJson } from './JsonCodec.js';

let $root = document.getElementById('hook') as any as HTMLElement;
if (!$root) throw Error('#root is missing');

let model : Model.T =
  localStorage.getItem('model') !== null
    ? loadJson(localStorage.getItem('model')!) as Model.T
    : { type : 'Model'
      , pools : []
      , movements : []
      , identifierCount : 0n
      };

function send(message : Message.T) {
  const now = BigInt(+Date.now());
  console.log('message:', message);
  model = Message.update(model, message, now);
  localStorage.setItem('model', dumpJson(model));
  rerender();
}

function rerender() {
  console.log('model:', model);
  const now = BigInt(+Date.now());
  $root.innerHTML = '';
  $root.append(Render.render(model, send, now));
}

rerender();

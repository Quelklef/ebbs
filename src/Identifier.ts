export type T = Identifier;

interface Identifier
  { type : 'Identifier'
  , value : string
  }

export function eq(a : Identifier, b : Identifier) : boolean {
  return a.value === b.value;
}

export function from(x : bigint) : Identifier {
  const digits = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  const base = BigInt(digits.length);
  let result = '';
  for (;;) {
    result = digits[Number(x % base)] + result;
    x = x / base;
    if (x <= 0) break;
  }
  return { type : 'Identifier', value : result };
}

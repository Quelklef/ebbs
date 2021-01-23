export function loadJson(json : string) : any {
  return JSON.parse(json, receiver);

  function receiver(_key : any, value : any) : any {
    if (typeof value === 'object'
       && value !== null
       && value.__json_type === 'bigint'
    ) {
      return BigInt(value.__json_payload);
    } else {
      return value;
    }
  }
}

export function dumpJson(value : any) : string {
  return JSON.stringify(value, replacer);

  function replacer(_key : any, value : any) : any {
    if (typeof value === 'bigint') {
      return { __json_type : 'bigint', __json_payload : value + '' };
    } else {
      return value;
    }
  }
}

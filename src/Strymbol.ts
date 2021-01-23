export type T = Strymbol;

type Strymbol = Sym | Str

interface Str
  { type : 'Str'
  , value : string
  }

interface Sym
  { type : 'Sym'
  , value : string
  }

export function Strymbol_eq(a : Strymbol, b : Strymbol) : boolean {
  return a.type === b.type && a.value === b.value;
}

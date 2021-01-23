export type T<T> = Maybe<T>;

type Maybe<T> = Just<T> | Nothing;

interface Just<T>
  { type : 'Just'
  , value : T
  }

interface Nothing
  { type : 'Nothing'
  }

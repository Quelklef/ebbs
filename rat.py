import functools as ft

def _calc_gcd(a, b):
  if b == 0: return a
  return _calc_gcd(b, a % b)

@ft.total_ordering
class Rat:
  """
  A rational number!
  """

  def __init__(self, *args):

    # Rat(Rat(22, 7))
    if len(args) == 1 and isinstance(args[0], Rat):
      num, den = args[0]._num, args[0]._den

    # Rat(22, 7)
    elif len(args) == 2 and isinstance(args[0], int) and isinstance(args[1], int):
      num, den = args

    # Rat(22)
    elif len(args) == 1 and isinstance(args[0], int):
      num, den = args[0], 1

    # Rat('3.14')
    elif len(args) == 1 and isinstance(args[0], str) and args[0].count('.') == 1:
      pre_str, suf_str = args[0].split('.')
      pre, suf = int(pre_str), int(suf_str)
      den = 10 ** len(suf_str)
      num = pre * den + suf

    # Rat('22/7')
    elif len(args) == 1 and isinstance(args[0], str) and args[0].count('/') == 1:
      num_str, den_str = args[0].split('/')
      num, den = int(num_str), int(den_str)

    # Rat('22')
    elif len(args) == 1 and isinstance(args[0], str):
      num, den = int(args[0]), 1

    else:
      raise ValueError(f"invalid rational construction Rat{repr(args)}")

    gcd = _calc_gcd(num, den)
    self._num = num // gcd
    self._den = den // gcd

  def __add__(self, other):
    other = Rat(other)
    return Rat(self._num * other._den + other._num * self._den, self._den * other._den)

  def __neg__(self):
    return Rat(-self._num, self._den)

  def __sub__(self, other):
    return self + -Rat(other)

  def __radd__(self, other):
    return self + other

  def __mul__(self, other):
    other = Rat(other)
    return Rat(self._num * other._num, self._den * other._den)

  def __rmul__(self, other):
    return self * other

  def __truediv__(self, other):
    other = Rat(other)
    return Rat(self._num * other._den, self._den * other._num)

  def floor(self):
    return self._num // self._den

  def __floordiv__(self, other):
    return (self / other).floor()

  def __mod__(self, other):
    return self - (self // other) * other

  def __eq__(self, other):
    other = Rat(other)
    return (self._num, self._den) == (other._num, other._den)

  def __lt__(self, other):
    other = Rat(other)
    return self._num * other._den < other._num * self._den

  def __abs__(self):
    return Rat(abs(self._num), abs(self._den))

  def __str__(self):
    if self._den == 1:
      return str(self._num)
    else:
      return f"{self._num}/{self._den}"

  @property
  def as_float(self):
    return self._num / self._den

  @property
  def approx(self):
    return format(self.as_float, '.2f')

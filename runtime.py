from sympy import Rational
import sympy as sp

class Transaction:
  def __init__(
    self,
    amount,
    distribution,
  ):
    self.amount = amount
    self.distribution = distribution

    area = sp.integrate(distribution, (sp.Symbol('x'), -sp.oo, sp.oo))
    if area != 1:
      raise ValueError(f"Given distribution is not normalized: {distribution}")

  def __getitem__(self, slice):
    return self.amount * sp.integrate(self.distribution, (sp.Symbol('x'), slice.start, slice.stop))

class Pool:
  def __init__(self, *,
    name,
    period,
    gain,
    capped,
  ):
    self.name = str(name)
    self.period = Rational(period)
    self.gain = Rational(gain)
    self.capped = bool(capped)

    # Running value
    self.value = Rational(0)
    # Transaction history
    self.history = []

  @property
  def rate(self):
    return self.gain / self.period

  def _simulate_idle(self, time_old, time_new):
    """ Simulate being idle over a given range of time """

    time_old = Rational(time_old)
    time_new = Rational(time_new)

    # Latent gain
    self.value += self.rate * (time_new - time_old)

    # Gain from transactions
    for tx in self.history:
      self.value += tx[time_old : time_new]

    if (self.capped and
         (  self.gain > 0 and self.value > self.gain
         or self.gain < 0 and self.value < self.gain)):
      self.value = self.gain

class Pools:
  def __init__(self):
    self._pools = dict()

  def new(self,
    name, *,
    period,
    gain,
    capped,
  ):
    if name in self._pools:
      raise ValueError(f"There is already a pool named '{name}'")

    pool = Pool(
      name = name,
      period = period,
      gain = gain,
      capped = capped,
    )
    self._pools[name] = pool

  def __getattr__(self, pool_name):
    try:
      return self._pools[pool_name]
    except KeyError:
      raise ValueError(f"No pool named '{pool_name}'")

  def __iter__(self):
    return iter(self._pools.values())

  def __len__(self):
    return len(self._pools)


# Convenince values for making times
from util import second, minute, hour, day, week, year, month
seconds = second
minutes = minute
hours   = hour
days    = day
weeks   = week
months  = month
years   = year

# Convenience values for making distributions
x = sp.Symbol('x')
# v Point distribution on [0, 0]
instant = sp.DiracDelta
# v Exponential distribution on [0, oo)
exponential = lambda var: sp.Piecewise((0, var < 0), (sp.exp(var), var >= 0))
# v Uniform distribution on [0, range]
uniform = lambda var, range: sp.Piecewise((0, var < 0), (0, var > range), (sp.Rational(1, range), True))

pools = Pools()

ebbs_time = 0

def pin(new_time):
  global ebbs_time
  for pool in pools:
    pool._simulate_idle(ebbs_time, new_time)
  ebbs_time = new_time

from sympy import Rational

class Pool:
  def __init__(self, *,
    name,
    period,
    gain,
    capped,
  ):
    self.name = str(name)
    self.value = Rational(0)
    self.period = Rational(period)
    self.gain = Rational(gain)
    self.capped = bool(capped)

  @property
  def rate(self):
    return self.gain / self.period

  def _simulate_idle(self, time_old, time_new):
    """ Simulate being idle over a given range of time """

    time_old = Rational(time_old)
    time_new = Rational(time_new)

    self.value += self.rate * (time_new - time_old)

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

pools = Pools()

ebbs_time = 0

def pin(new_time):
  global ebbs_time
  for pool in pools:
    pool._simulate_idle(ebbs_time, new_time)
  ebbs_time = new_time

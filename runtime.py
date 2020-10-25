from rat import Rat
import time as time_module

class Pool:
  def __init__(self, *,
    name,
    value,
    period,
    gain,
    rollover,
    gradual,
  ):
    self.name = str(name)
    self.value = Rat(value)
    self.period = Rat(period)
    self.gain = Rat(gain)
    self.rollover = bool(rollover)
    self.gradual = bool(gradual)

  @property
  def rate(self):
    return self.gain / self.period

  def _simulate_idle(self, time_old, time_new):
    """ Simulate being idle over a given range of time """

    time_old = Rat(time_old)
    time_new = Rat(time_new)

    if (self.rollover, self.gradual) == (False, False):
      if time_new // self.period > time_old // self.period:
        self.value = self.gain

    elif (self.rollover, self.gradual) == (True, False):
      self.value += (time_new // self.period - time_old // self.period) * self.gain

    elif (self.rollover, self.gradual) == (False, True):
      if time_new // self.period > time_old // self.period:
        self.value = self.gain * ((time_new % self.period) / self.period)

    elif (self.rollover, self.gradual) == (True, True):
      self.value += (time_new - time_old) * self.rate

  def __str__(self):
    return f"{self.name}: {self.value.approx}, +{self.rate.approx}"


class Pools:
  def __init__(self):
    self._pools = dict()

  def new(self,
    name, *,
    period,
    gain,
    rollover,
    gradual,
  ):
    if name in self._pools:
      raise ValueError(f"There is already a pool named '{name}'")

    pool = Pool(
      name = name,
      value = 0,
      period = period,
      gain = gain,
      rollover = rollover,
      gradual = gradual,
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

  def __str__(self):
    return "\n".join(str(pool) for pool in self._pools.values())


# Convenince values for making tims
seconds = second = 1
minutes = minute = second * 60
hours   = hour   = minute * 60
days    = day    = hour   * 24
weeks   = week   = day    * 7
years   = year   = day    * Rat('365.2422')  # the average year
months  = month  = day    * Rat('365/12')  # the average month

pools = Pools()

time = 0

def pin_now():
  global time
  now = int(time_module.time())
  for pool in pools:
    pool._simulate_idle(time, now)
  time = now

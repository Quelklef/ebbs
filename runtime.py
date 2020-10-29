from sympy import Rational
import sympy as sp
t = sp.Symbol('t')

def _integrate(expr, opts):
  # For some reason, sympy's builtin definite integration is being very slow.
  # In particular, it chokes on stuff like exp(t - 100000000), even with manual=True.
  # Unfortunately, these are exactly the kinds of functions that we want to integrate.
  # This function is to counteract that. It does definite integration by evaluating
  # the indefinite integral at its endpoints.
  var, lo, hi = opts
  i = sp.integrate(expr, var)
  # v Define Heaviside(0) = 0 so that the integral of DiracDelta over [0, oo) is 1
  i = i.replace(sp.Heaviside(0), 0)
  return i.subs(var, hi) - i.subs(var, lo)

class Pool:
  def __init__(self, name, *, rate, cap, canonical_period):
    self.name = name
    self.rate = rate
    self.cap = cap
    self.canonical_period = canonical_period

    # Running value
    self.value = Rational(0)
    # Transaction history
    self.history = []

  def modified_rate(self, time):
    """ Return the pool rate, taking into account ongoing transactions """
    return self.rate + sum(tx.subs(t, time) for tx in self.history)

  def transact(self, *, amount, distn, time):
    area = _integrate(distn, (t, -sp.oo, sp.oo))
    if area != 1:
      raise ValueError(f"Given distribution is not normalized: {distn}")

    # Amplify the distribution by `amount` and shift it to its time
    final_distn = amount * distn.replace(t, t - time)
    self.history.append(final_distn)

  def _simulate_idle(self, time_old, time_new):
    """ Simulate being idle over a given range of time """

    time_old = Rational(time_old)
    time_new = Rational(time_new)

    # Latent gain
    self.value += self.rate * (time_new - time_old)

    # Gain from transactions
    for tx in self.history:
      self.value += _integrate(tx, (sp.Symbol('t'), time_old, time_new))

    if (self.cap is not None and
         (  self.cap > 0 and self.value > self.cap
         or self.cap < 0 and self.value < self.cap)):
      self.value = self.cap

class Pools:
  def __init__(self):
    self._pools = dict()

  def new(self, name, *args, **kwargs):
    if name in self._pools:
      raise ValueError(f"There is already a pool named '{name}'")
    pool = Pool(name, *args, **kwargs)
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
# v Point distribution on [0, 0]
instant = sp.DiracDelta(t)
# v Exponential decay on [0, oo)
decay = lambda k: sp.Piecewise((0, t < 0), (sp.exp(-t / k) / k, t >= 0))
# v Uniform distribution on [0, range]
uniform = lambda k: sp.Piecewise((0, t < 0), (0, t > k), (sp.Rational(1, k), True))

pools = Pools()

ebbs_time = -sp.oo

def pin(new_time):
  global ebbs_time
  for pool in pools:
    pool._simulate_idle(ebbs_time, new_time)
  ebbs_time = new_time

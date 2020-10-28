import os
import sys
import time
import traceback
import sympy as sp

import util

from runtime import *

diary_file_path = 'diary.py'

if not os.path.exists('diary.py'):
  with open('diary.py', 'w') as diary_file:
    diary_file.write("from runtime import *\n")

diary_file = open(diary_file_path, 'a')

import diary

def now():
  return int(time.time())


# -- Parsers and unevalrs -- #

def parse_pool_name(string):
  allowed = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890")
  invalid = ','.join(set(string) - allowed)
  if invalid:
    raise ValueError(f"The following chars are not allowed in pool names: {invalid}")
  return string

def parse_rational(string):
  if '/' in string:
    num, den = string.split('/')
    num, den = int(num), int(den)
    return sp.Rational(num, den)
  else:
    val = int(string)
    return sp.Integer(val)

def uneval(value):
  """ Return python code evaluating to a given value """
  # Most of the time, a class's __repr__ method will implement this.
  # However, sympy doesn't seem to follow this convention.
  defer_to_repr = [str, int]
  if any(isinstance(value, t) for t in defer_to_repr):
    return repr(value)
  elif isinstance(value, sp.Integer):
    return f"Rational({value})"
  elif isinstance(value, sp.Rational):
    num, den = str(value).split('/')
    return f"Rational({num}, {den})"
  else:
    raise ValueError(f"Don't know how to uneval type {type(value)}")


# -- CLI -- #

def _run(user_command, *, desc=None, record=True):
  desc = desc or "(no description)"
  full_command = f'\n# {desc}\npin({now()})\n{user_command}\n'
  try:
    exec(full_command, globals())
  except Exception:
    traceback.print_exc()
    raise
  else:
    if record:
      diary_file.write(full_command)

def view():
  """
  View the current state.
  """
  if len(pools) == 0:
    print("No pools. Use `ebbs new` to make one.")
  else:
    pin(now())
    print(util.format_grid([
      [ f"{pool.name}:"
      , round(pool.value, 2)
      , f"(+{round(pool.gain, 2)}/{util.format_as_duration(pool.period)})"
      , ' '*5
      , f"[{(not pool.capped) * 'not '}capped]"
      ] for pool in pools
    ]))

def do(command, *, desc, record=True):
  """
  Run a raw Python command
  :param command: the command
  :param desc: description
  :param record: append the command to the diary  [default: True]
  """
  command = str(command)
  desc = str(desc)
  record = bool(record)

  _run(command, desc=desc, record=record)

def new(name, *, period, gain, capped):
  """
  Create a new pool.
  :param name: The pool name
  :param period:
    The canonical period for the pool, e.g. '3*months'.
    This has no effect on calculations.
  :param gain: The value gained per period, e.g. '300'.
  :param capped:
    True iff the pool value is to never exceed `gain` (in magnitude).
    This is the continuous analog to having a non-rollover discrete pool.
  """
  name = parse_pool_name(name)
  period = str(period)
  gain = parse_rational(gain)
  capped = bool(capped)

  _run(f"pools.new({uneval(name)}, period={period}, gain={uneval(gain)}, capped={uneval(capped)})")
  view()

def t(pool, amount, *, desc, distn="instant(x)"):
  """
  Record a transaction with a pool.
  :param pool: the name of the pool
  :param amount: the value delta
  :param desc: transaction description
  :param distn: transaction distribution  [default: "instant(x)"]
  """
  pool = parse_pool_name(pool)
  amount = parse_rational(amount)
  desc = str(desc)
  distn = str(distn)

  shifted_distn = f"sp.Lambda(x, {distn})(x - {now()})"
  _run(f"pools.{pool}.history.append(Transaction({uneval(amount)}, {shifted_distn}))", desc=desc)
  view()


# -- Main -- #

import fire

# monkeypatch fire to not parse arguments and instead leave everything as a string
fire.parser.DefaultParseValue = lambda x: x

fire.Fire()


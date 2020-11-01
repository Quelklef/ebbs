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

# -- Parsers and unevalrs -- #

def parse_pool_name(string):
  allowed = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890")
  invalid = ','.join(set(string) - allowed)
  if invalid:
    raise ValueError(f"The following chars are not allowed in pool names: {invalid}")
  return string

def parse_decimal(string):
  """ Parses e.g. 1.5 into Rational(3, 2) """
  if '.' in string:
    whole, frac = string.split('.')
    base = 10 ** len(frac)
    return sp.Rational(int(whole) * base + int(frac), base)
  else:
    return sp.Integer(int(string))

def parse_rational(string):
  """ Parses e.g. 3.5/4 into Rational(7, 8) """
  if '/' in string:
    num, den = string.split('/')
    return sp.Rational(parse_decimal(num), parse_decimal(den))
  else:
    return parse_decimal(string)

def uneval(value):
  """ Return python code evaluating to a given value """
  # Most of the time, a class's __repr__ method will implement this.
  # However, sympy doesn't seem to follow this convention.
  defer_to_repr = [str, int, bool, type(None)]
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
  full_command = f'\n# {desc}\npin({util.now()})\n{user_command}\n'
  try:
    exec(full_command, globals())
  except Exception:
    raise
  else:
    if record:
      diary_file.write(full_command)

def view(*, project='0', period=None):
  """
  View the current state.
  :param project: if given, will show the result not for the current time but for some time into the future, e.g. '1*day'
  :param period: if given and nonzero, the period to use when showing rates
  """
  project = str(project)
  period = None if period is None else eval(period)

  if len(pools) == 0:
    print("No pools. Use `ebbs new` to make one.")
  else:
    now_time = util.now() + eval(project)
    pin(now_time)

    def pool_to_row(pool):
      return (
        [ f"{pool.name}"
        , ":"
        , round(pool.value, 2) if pool.value <= pool.max_display_val else '>' + str(round(pool.max_display_val), 2)
        , util.format_rate(pool.modified_rate(now_time), period or pool.canonical_period)
        , (pool.modified_rate(now_time) != pool.rate) * f"(base {util.format_rate(pool.rate, period or pool.canonical_period)})"
        ])

    grid = list(map(pool_to_row, pools))
    print(util.format_grid(grid))

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

def new(name, *, rate, cap):
  """
  Create a new pool.
  :param name: The pool name
  :param rate:
    The pool rate, e.g. '100 per 2*months'
  :param cap:
    The pool cap (maximum value), if there is one.
    This is the continuous analog to being non-rollover.
    If 'none', no cap.
    If 'gain', the cap is the rate gain.
    If a number, that number is the cap.
  """
  name = parse_pool_name(name)
  gain, canonical_period = rate.split(' per ')
  gain = parse_rational(gain)
  rate = Rational(gain, eval(canonical_period))
  cap = (None if cap == 'none'
        else gain if cap == 'gain'
        else parse_rational(cap))

  _run(f"pools.new({uneval(name)}, rate={uneval(rate)}, canonical_period={canonical_period}, cap={uneval(cap)})")
  view()

def tn(pool, amount, *, desc, distn="instant"):
  """
  Record a transaction with a pool.
  :param pool: the name of the pool
  :param amount: the value delta
  :param desc: transaction description
  :param distn: transaction distribution  [default: "instant"]
  """
  pool = parse_pool_name(pool)
  amount = parse_rational(amount)
  desc = str(desc)
  distn = str(distn)

  _run(f"pools.{pool}.transact(amount={uneval(amount)}, time={util.now()}, distn={distn})", desc=desc)
  view()


# -- Main -- #

import fire

# monkeypatch fire to not parse arguments and instead leave everything as a string
fire.parser.DefaultParseValue = lambda x: x

fire.Fire()


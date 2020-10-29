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

def view():
  """ View the current state. """
  if len(pools) == 0:
    print("No pools. Use `ebbs new` to make one.")
  else:
    pin(util.now())

    def pool_to_row(pool):
      return (
        [ f"{pool.name}"
        , ":"
        , round(pool.value, 2)
        , util.format_rate(pool.modified_rate(util.now()), pool.canonical_period)
        , (pool.modified_rate(util.now()) != pool.rate) * f"(base {util.format_rate(pool.rate, pool.canonical_period)})"
        , (pool.cap is not None) * f"[cap={pool.cap}]"
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

  shifted_distn = f"({distn}).replace(t, t - {util.now()})"
  _run(f"pools.{pool}.transact({uneval(amount)}, {shifted_distn})", desc=desc)
  view()


# -- Main -- #

import fire

# monkeypatch fire to not parse arguments and instead leave everything as a string
fire.parser.DefaultParseValue = lambda x: x

fire.Fire()


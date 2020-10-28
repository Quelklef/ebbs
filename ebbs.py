import os
import sys
import time
import traceback

import fire

import util

from runtime import *

diary_file_path = 'diary.py'

if not os.path.exists('diary.py'):
  with open('diary.py', 'w') as diary_file:
    diary_file.write("from runtime import *\n")

import diary

# --

class CLI:

  def __init__(self, diary_file):
    self.diary_file = diary_file

  def _run(self, user_command, *, desc=None, record=True):
    desc = desc or "(no description)"
    now = int(time.time())
    full_command = f'\n# {desc}\npin({now})\n{user_command}\n'
    try:
      exec(full_command, globals())
    except Exception:
      traceback.print_exc()
    else:
      if record:
        self.diary_file.write(full_command)

  def _pin_now(self):
    now = int(time.time())
    pin(now)

  def view(self):
    """
    View the current state.
    """
    if len(pools) == 0:
      print("No pools. Use `ebbs new_pool` to make one.")
    else:
      self._pin_now()
      print(util.format_grid([
        [ f"{pool.name}:"
        , format(pool.value.evalf(), '.2f')
        , f"(+{pool.gain}/{util.format_as_duration(pool.period)})"
        , ' '*5
        , f"[{(not pool.capped) * 'not '}capped]"
        ] for pool in pools
      ]))

  def do(self, command, *, desc, record=True):
    """
    Run a raw Python command
    :param command: the command
    :param desc: description
    :param record: append the command to the diary  [default: True]
    """
    self._run(command, desc=desc, record=record)

  def new_pool(self,
    name, *,
    period,
    gain,
    capped,
  ):
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
    self._run(f"pools.new({repr(name)}, period={period}, gain={repr(gain)}, capped={repr(capped)})")
    self.view()

  def t(self, pool, amount, *, desc, distn="instant(x)"):
    """
    Record a transaction with a pool.
    :param amount: the value delta
    :param pool: the name of the pool
    :param desc: transaction description
    :param distn: transaction distribution  [default: "instant(x)"]
    """
    now = int(time.time())
    shifted_distn = f"sp.Lambda(x, {distn})(x - {now})"
    self._run(f"pools.{pool}.history.append(Transaction({amount}, {shifted_distn}))", desc=desc)
    self.view()

with open(diary_file_path, 'a') as diary_file:
  fire.Fire(CLI(diary_file))


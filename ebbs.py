import os
import sys
import traceback
import time as time_module

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
    full_command = f'\n# {desc}\npin_now()\n{user_command}\n'
    try:
      exec(full_command, globals())
    except Exception:
      traceback.print_exc()
    else:
      if record:
        self.diary_file.write(full_command)

  def view(self):
    """
    View the current state.
    """
    if len(pools) == 0:
      print("No pools. Use `ebbs new_pool` to make one.")
    else:
      pin_now()
      print(util.format_grid([
        [ f"{pool.name}:"
        , pool.value
        , f"(+{pool.gain}/{util.format_as_duration(pool.period)})"
        , ' '*5
        , f"[{(not pool.rollover) * 'no '}rollover; {(not pool.gradual) * 'not '}gradual]"
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
    rollover,
    gradual,
    initial_value = 0,
  ):
    """
    Create a new pool.
    :param name: the pool name
    :param period: the gain/rollover period for the pool, e.g. '3*months'
    :param gain: the value gained per period, e.g. '300'  [default: 0]
    :param rollover: true iff the pool value persists at the beginning of a period  [default: True]
    :param gradual: true iff value is the gained continuously over a period rather than all at the beginning  [default: True]
    """
    self._run(f"pools.new('{name}', period={period}, gain={gain}, rollover={rollover}, gradual={gradual})")

  def take(self, amount, *, out_of, desc):
    """
    Record a loss of value out of a pool.
    :param amount: the amount lost
    :param out_of: the name of the pool
    """
    self._run(f"pools.{out_of}.value -= {amount}", desc=desc)

  def put(self, amount, *, into, desc):
    """
    Record a gain of value into a pool.
    :param amount: the amount gained
    :param into: the name of the pool
    """
    self._run(f"pools.{into}.value += {amount}", desc=desc)

with open(diary_file_path, 'a') as diary_file:
  fire.Fire(CLI(diary_file))


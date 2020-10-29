from sympy import Rational

import time

"""
Time constants, defined as:
  a minute is 60 seconds
  an hour is 60 minutes
  a day is 24 hours
  a week is 7 days
  a year is 365.2422 days (the average year)
  a month is 1/12 of a year (the average month)

Time values are reified at runtime as python/sympy
numbers, where the unit is *year*. In other words, the
integer `10` represents 10 years.

If this seems silly, that's fair. I would rather do it
in terms of seconds or minutes. However, for some reason
sympy chokes when doing integration that involves large
constants, but not when doing integration that involves
rational numbers with large denominators. So, here we are.
"""
year = Rational(1)
month = year / 12
day = year / Rational('365.2422')
week = day * 7
hour = day / 24
minute = hour / 60
second = minute / 60

def now():
  return int(time.time()) * second

def format_as_duration(time_delta):
  """
  Format a time delta (in seconds) as a nicely-readable duration
  """
  time_delta = Rational(time_delta)
  units = [(second, 'sec'), (minute, 'min'), (hour, 'hr'), (day, 'day'), (week, 'wk'), (month, 'mo'), (year, 'yr'), (float('inf'), 'X')]
  for (unit_val, unit_tag), (next_unit_val, _) in zip(units, units[1:]):
    if time_delta < next_unit_val:
      as_unit = time_delta / unit_val
      # Format e.g. '60' as 'm' instead of '1m'
      if as_unit == 1:
        return unit_tag
      else:
        return format(as_unit.evalf(), '.2g') + unit_tag

def format_grid(grid):
  """
  Given a square 2d grid of values, treat each entry as a row
  and format the grid to a string, aligning columns.
  """

  # handle corner case of no rows or no columns
  if not grid or not grid[0]:
    return ""

  col_separator = ' '
  col_count = len(grid[0])

  # convert all to string
  grid = [[str(val) for val in row] for row in grid]

  # find column widths
  col_widths = [max(len(row[i]) for row in grid) for i in range(col_count)]

  # format
  format_row = lambda row: col_separator.join(row[i].ljust(col_widths[i]) for i in range(col_count))
  formatted = '\n'.join(map(format_row, grid))

  return formatted

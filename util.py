from rat import Rat

second = 1
minute = second * 60
hour   = minute * 60
day    = hour   * 24
week   = day    * 7
year   = day    * Rat('365.2422')  # the average year
month  = year   / 12  # the average month

def format_as_duration(time_delta):
  """
  Format a time delta (in seconds) as a nicely-readable duration
  """
  time_delta = Rat(time_delta)
  units = [(second, 's'), (minute, 'm'), (hour, 'h'), (day, 'd'), (week, 'w'), (month, 'm'), (year, 'y'), (float('inf'), 'X')]
  for (unit_val, unit_tag), (next_unit_val, _) in zip(units, units[1:]):
    if time_delta < next_unit_val:
      as_unit = time_delta / unit_val
      # Format e.g. '60' as 'm' instead of '1m'
      if as_unit == Rat(1):
        return unit_tag
      else:
        return format(as_unit.as_float, '.2g') + unit_tag

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

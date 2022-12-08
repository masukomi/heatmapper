#!/usr/bin/env ruby

# https://github.com/wistia/heatmap-palette/blob/master/README.md
# \x1b[38;2;R;G;Bm

COLOR_SCHEMES={
  :wistia => [
    # *Honeysuckle*
    [228, 255, 122],
    # *Broom*
    [255, 232, 25],
    # *Selective Yellow*
    [255, 188, 3],
    # *Orange Peel*
    [254, 160, 0],
    # *Flush Orange*
    [252, 126, 0]
  ]
}

def percentify_numbers(numbers)
  output = []
  max = numbers.max
  max_f = max.to_f
  numbers.each do |n|
    output << ((n.to_f / max_f) * 100).round
  end
  output
end
def prep_output_data(rows=7)
  output_data = []
  rows.times do
    output_data.push []
  end
  output_data
end

def populate_output_data(output_data, p_numbers, rows, columns)
  row_index = 0
  p_numbers.each do |num|
    output_data[row_index].push num
    row_index = row_index == (rows - 1) ? 0 : row_index + 1;
    break if row_index == 0 && output_data[row_index].size == columns
  end
  output_data
end
def print_number(num, color_scheme=:wistia)
  # num is a percentage
  fifth = num == 100 ? 4 : (num / 20);
  color_string=COLOR_SCHEMES[color_scheme][fifth].join(';')
  # You make a square grid by printing two box characters (or two spaces).
  printf("\x1b[38;2;#{color_string}m██")
end

# default_color=[0,0,0] # TODO parameterize
rows = 7 # TODO parameterize
columns = `tput cols`.strip.to_i
number_type = :relative # TODO paramaterize for :absolute
numbers = ARGF.read.split(/\s+/).map{ |n| n.to_i }
# STDERR.puts("XXX numbers: #{numbers.inspect}")

p_numbers = percentify_numbers(numbers)
output_data = prep_output_data(rows)

output_data = populate_output_data(output_data, p_numbers, rows, columns)
output_data.each do | row |
  row.each do | n |
    print_number(n)
  end
  printf "\x1b[0m\n"
end

# load libraries
library(tidyverse)

# read data
data <- read_table(file = './2022/data/10.txt', col_names = c('cmd', 'value'))

# add value at the end of each cycle
data <- data |> 
  mutate(
    cycle = cumsum(if_else(cmd == 'noop', 1, 2)),
    value = cumsum(coalesce(value, 0)) + 1
  ) |> 
  complete(cycle = seq_len(max(cycle))) |> 
  fill(value, .direction = 'down')
  
# 1st problem
data |> 
  filter(cycle %in% seq(19, 219, by = 40)) |> 
  summarise(sum((cycle+1) * value)) |> 
  pull()

# 2nd problem
data |> 
  mutate(
    row = ceiling(cycle / 40),
    col = rep(0:39, nrow(data)/40),
    char = if_else(abs(coalesce(lag(value), 1) - col) <= 1, '#', ' ')
  ) |> 
  pivot_wider(id_cols = row, names_from = col, values_from = char) |> 
  unite(col = 'string', -row, sep = "")

# load libraries
library(tidyverse)

# read data
data <- read_csv(file = './2022/data/8.txt', col_names = 'col', col_types = 'c')

# reshape
data <- data |> 
  separate(col, into = paste0(1:(nrow(data))), sep = 1:(nrow(data))) |> 
  mutate(row = row_number(), .before = everything()) %>%
  pivot_longer(cols = -row, names_to = "col") |> 
  mutate(across(where(is.character), as.integer))

# add maximum tree height from every side until the current tree
data <- data |> 
  group_by(row) |> 
  arrange(desc(col)) |> 
  mutate(max_col_desc = lag(cummax(value), default = -1)) |> 
  arrange(col) |> 
  mutate(max_col_asc  = lag(cummax(value), default = -1)) |> 
  group_by(col) |> 
  arrange(desc(row)) |> 
  mutate(max_row_desc = lag(cummax(value), default = -1)) |> 
  arrange(row) |> 
  mutate(max_row_asc  = lag(cummax(value), default = -1)) |> 
  ungroup()

# 1st problem
data |> 
  summarise(sum(value > pmap_dbl(across(starts_with('max_')), min))) |> 
  pull()

# 2nd problem
data
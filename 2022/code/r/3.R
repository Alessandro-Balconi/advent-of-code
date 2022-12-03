# load libraries
library(tidyverse)

# read data
data <- read_delim(
  file = './2022/data/3.txt', 
  delim = '\n',
  col_names = 'value',
  col_types = 'c'
)

# 1st problem
data |> 
  # split into 2 columns at half string length
  transmute(
    first = str_sub(value, end = nchar(value) / 2),
    second = str_sub(value, start = (nchar(value) / 2)+1),
  ) |> 
  mutate(
    # split strings into list of letters
    across(.cols = everything(), .fns = str_split, pattern = ""),
    # intersect the 2 sets of letters to find the letters in both halves
    both = map2_chr(.x = first, .y = second, .f = intersect),
    # calculate the priority score of each letter
    priority = map_int(.x = both, .f = grep, x = c(tolower(LETTERS), LETTERS))
  ) |> 
  # summarise total priorty score
  summarise(sum(priority)) |> 
  pull()

# 2nd problem
data |> 
  # define groups and reshape data
  mutate(
    group = rep(1:(nrow(data)/3), each = 3),
    pos = rep(1:3, nrow(data)/3)
  ) |> 
  pivot_wider(names_from = pos, values_from = value, names_prefix = 'pos_') |> 
  # same as 1st half
  mutate(
    across(.cols = starts_with('pos_'), .fns = str_split, pattern = ""),
    pos_12 = map2(.x = pos_1, .y = pos_2, .f = intersect),
    all = map2_chr(.x = pos_12, .y = pos_3, .f = intersect),
    priority = map_int(.x = all, .f = grep, x = c(tolower(LETTERS), LETTERS))
  ) |> 
  summarise(sum(priority)) |> 
  pull()

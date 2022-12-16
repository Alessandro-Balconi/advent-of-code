# load libraries
library(tidyverse)
library(cppRouting)

# read data
data <- read_table(
  file = './2022/data/12.txt', 
  col_names = FALSE, 
  col_types = 'c'
)

# reshape
data <- data |> 
  separate(
    col = X1, 
    into = paste0(1:(max(nchar(data$X1)))), 
    sep = 1:max(nchar(data$X1))
  ) |> 
  mutate(y = row_number()) |> 
  pivot_longer(cols = -y, names_to = 'x', names_transform = as.integer) |> 
  mutate(
    id = row_number(),
    value_num = case_when(
      value == "S" ~ 1,
      value == "E" ~ 26,
      TRUE ~ map_dbl(.x = value, .f = match, table = letters)
    )
  )

# convert into graph roads
roads <- data %>%
  full_join(., ., by = character(), suffix = c('_from', '_to')) |> 
  filter(
    abs(x_from - x_to) + abs(y_from - y_to) == 1,
    value_num_to - value_num_from <= 1
  ) |> 
  select(from = id_from, to = id_to) |> 
  mutate(cost = 1)

# 1st problem
roads |> 
  makegraph() |> 
  get_path_pair(
    from = data$id[data$value == 'S'], 
    to = data$id[data$value == 'E'],
    long = TRUE
  ) |> 
  nrow() - 1

# 2nd problem
roads |> 
  makegraph() |> 
  get_multi_paths(
    from = data$id[data$value_num == 1], 
    to = data$id[data$value == 'E'],
    long = TRUE
  ) |> 
  count(from, sort = TRUE) |> 
  summarise(min(n-1)) |> 
  pull()

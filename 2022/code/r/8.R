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

# 1st problem
# calculate maximum tree height from every side until the current tree
# a tree is visible if its value is higher than at least one of the maxes
data |> 
  group_by(row) |> 
  mutate(max_col_asc  = lag(cummax(value), default = -1)) |>
  arrange(desc(col)) |> 
  mutate(max_col_desc = lag(cummax(value), default = -1)) |> 
  group_by(col) |> 
  mutate(max_row_asc  = lag(cummax(value), default = -1)) |> 
  arrange(desc(row)) |> 
  mutate(max_row_desc = lag(cummax(value), default = -1)) |> 
  ungroup() |> 
  summarise(sum(value > pmap_dbl(across(starts_with('max_')), min))) |> 
  pull()

# 2nd problem
# function to calculate the scenic score of a given position
get_scenic_score <- function(row, col, value){
  
  count_visible <- function(v){ min(sum(cumsum(value <= v) == 0)+1, length(v)) }
  
  right <- count_visible(data$value[data$row == row & data$col > col])
  left  <- count_visible(rev(data$value[data$row == row & data$col < col]))
  up    <- count_visible(data$value[data$col == col & data$row > row])
  down  <- count_visible(rev(data$value[data$col == col & data$row < row]))
  
  return(left * right * up * down)
  
}

# get max of all the scenic scores
max(pmap_dbl(data, get_scenic_score))

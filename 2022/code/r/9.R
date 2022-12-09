# load libraries
library(tidyverse)

# read data
data <- read_delim(
  file = './2022/data/9.txt', 
  delim = " ",
  col_names = c('dir', 'n'), 
  col_types = 'ci'
)

# function that iteratively moves each knot
move_knots <- function(data, n_knots){
  
  for (k in seq_len(n_knots)){
    
    head_x <- data$x[data$knot == k-1]
    head_y <- data$y[data$knot == k-1]
    tail_x <- tail_y <- rep(0, length(head_x))
    
    for (i in 2:length(head_x)) {
      
      tail_x[i] <- case_when(
        abs(head_x[i] - tail_x[i-1]) ==  2 ~ tail_x[i-1] + sign(head_x[i] - tail_x[i-1]),
        abs(head_y[i] - tail_y[i-1]) < 2 ~ tail_x[i-1],
        TRUE ~ head_x[i]
      )
      
      tail_y[i] <- case_when(
        abs(head_y[i] - tail_y[i-1]) ==  2 ~ tail_y[i-1] + sign(head_y[i] - tail_y[i-1]),
        abs(head_x[i] - tail_x[i-1]) < 2 ~ tail_y[i-1],
        TRUE ~ head_y[i]
      )
      
    }
    
    data <- tibble(knot = k, x = tail_x, y = tail_y) |> 
      bind_rows(data)
    
  }
  
  return(data)
  
}

# 1st&2nd problem; ~12 seconds to run, how to make quicker?
# calculate the coordinates of the head, starting from (0,0)
data |> 
  uncount(n) |> 
  add_row(dir = 's', .before = 1) |> 
  mutate(
    x = cumsum((dir == "R") - (dir == "L")) + 0,
    y = cumsum((dir == "U") - (dir == "D")) + 0,
    knot = 0
  ) |> 
  move_knots(n_knots = 9) |>
  filter(knot %in% c(1, 9)) |> 
  group_by(knot) |> 
  summarise(n_distinct(x, y), .groups = 'drop')

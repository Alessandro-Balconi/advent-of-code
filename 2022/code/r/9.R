# load libraries
library(tidyverse)

# read data
data <- read_delim(
  file = './2022/data/9.txt', 
  delim = " ",
  col_names = c('dir', 'n'), 
  col_types = 'ci'
)

# calculate the coordinates of the head and of each node, starting from (0,0)
data <- data |> 
  uncount(n) |> 
  add_row(dir = 's', .before = 1) |> 
  mutate(
    knot = 0,
    step = row_number(),
    x = cumsum((dir == "R") - (dir == "L")),
    y = cumsum((dir == "U") - (dir == "D"))
  )

# wip ----

n_knots = 3

move_knots <- function(data, n_knots){
  
  knots <- tibble(
    dir = rep(data$dir, n_knots),
    knot = rep(seq_len(n_knots), each = length(data$dir)),
    step = rep(seq_len(length(data$dir)), n_knots),
    x = 0,
    y = 0
  ) %>%
    bind_rows(data, .)
  
  for (k in seq_len(n_knots)){
    
    for (i in 2:nrow(data)) {
    
    tail_x[i] <- case_when(
      head_x[i] - tail_x[i-1] ==  2 ~ tail_x[i-1] + 1L,
      head_x[i] - tail_x[i-1] == -2 ~ tail_x[i-1] - 1L,
      abs(head_y[i] - tail_y[i-1]) < 2 ~ tail_x[i-1],
      TRUE ~ head_x[i]
    )
    
    tail_y[i] <- case_when(
      head_y[i] - tail_y[i-1] ==  2 ~ tail_y[i-1] + 1L,
      head_y[i] - tail_y[i-1] == -2 ~ tail_y[i-1] - 1L,
      abs(head_x[i] - tail_x[i-1]) < 2 ~ tail_y[i-1],
      TRUE ~ head_y[i]
    )
    
    }
    
  }
  
}

# part 1 solver ----
library(tidyverse)

# read data
data <- read_delim(
  file = './2022/data/9.txt', 
  delim = " ",
  col_names = c('dir', 'n'), 
  col_types = 'ci'
)

# function to moves the tail according to head's movement
move_tail <- function(dir, head_x, head_y){
  
  tail_x <- tail_y <- rep(0L, length(head_x))
  
  for (i in 2:length(head_x)) {
    
    tail_x[i] <- case_when(
      head_x[i] - tail_x[i-1] ==  2 ~ tail_x[i-1] + 1L,
      head_x[i] - tail_x[i-1] == -2 ~ tail_x[i-1] - 1L,
      abs(head_y[i] - tail_y[i-1]) < 2 ~ tail_x[i-1],
      TRUE ~ head_x[i]
    )
    
    tail_y[i] <- case_when(
      head_y[i] - tail_y[i-1] ==  2 ~ tail_y[i-1] + 1L,
      head_y[i] - tail_y[i-1] == -2 ~ tail_y[i-1] - 1L,
      abs(head_x[i] - tail_x[i-1]) < 2 ~ tail_y[i-1],
      TRUE ~ head_y[i]
    )
    
  }
  
  return(bind_cols(x = tail_x, y = tail_y))
  
}

# calculate the coordinates of head and tail, starting from (0,0)
data <- data |> 
  uncount(n) |> 
  add_row(dir = 's', .before = 1) |> 
  mutate(
    head_x = cumsum((dir == "R") - (dir == "L")),
    head_y = cumsum((dir == "U") - (dir == "D")),
    tail = move_tail(dir, head_x, head_y)
  ) |> 
  unnest(tail, names_sep = "_")

# 1st problem
data |> 
  distinct(tail_x, tail_y) |> 
  nrow()

# 2nd problem
data
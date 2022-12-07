# load libraries
library(tidyverse)

# read data
data <- read_csv(
  file = './2022/data/5.txt',
  col_names = FALSE,
  trim_ws = FALSE,
  n_max = 8
)

# split each string into N columns (one for each character)
# and keep only the columns with letters
ncols <- max(nchar(data$X1))

data <- data |> 
  separate(X1, into = paste0(seq_len(ncols)), sep = seq_len(ncols - 1)) %>%
  select(where(~sum(match(., LETTERS), na.rm = TRUE) > 0)) %>%
  rename_with(function(x) seq_len(ncol(.)))

# reshape data in a way that makes sense to me
data <- data |> 
  mutate(rank = as.double(seq(from = n(), to = 1))) |> 
  pivot_longer(cols = -rank, names_to = "pos", names_transform = as.double) |> 
  filter(value != " ")

# read crate moves to do
moves_to_do <- read_delim(
  file = './2022/data/5.txt',
  delim = ' ',
  col_names = FALSE,
  skip = 9
) |> 
  select(amount = X2, from = X4, to = X6)

# 1st&2nd problem
# function that moves stuff around
move_crates <- function(data, amount, from, to){
  
  max_to <- data |> 
    filter(pos == to) |> 
    summarise(max(c(rank, 0), na.rm = TRUE)) |> 
    pull()

  max_from <- data |> 
    filter(pos == from) |> 
    summarise(max(rank, na.rm = TRUE)) |> 
    pull()
  
  data <<- data |> 
    mutate(
      new_rank = if_else(
        pos == from & rank > max_from - amount, 
        # 1st problem
        #max_to + max_from - rank + 1,
        #2nd problem
        max_to + amount + rank - max_from,
        rank
      ),
      pos = if_else(
        pos == from & rank > max_from - amount, 
        to, 
        pos
      )
    ) |> 
    select(rank = new_rank, pos, value)
  
}

# iterate function over all moves that need to be done
for (i in seq_len(nrow(moves_to_do))) {
  
  move_crates(
    data = data,
    amount = moves_to_do$amount[i],
    from = moves_to_do$from[i],
    to = moves_to_do$to[i]
  )
  
}

# get letter on top of each group
data |> 
  group_by(pos) |> 
  slice_max(n = 1, order_by = rank) |> 
  pull(value) |> 
  paste0(collapse = '')

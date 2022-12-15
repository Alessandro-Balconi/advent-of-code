# load libraries
library(tidyverse)

# read data
data <- read_delim(
  file = './2022/data/11.txt', 
  delim = ':', 
  col_names = FALSE,
  col_types = 'c'
)

# preprocess this file to make it usable
data <- data |> 
  mutate(monkey = parse_number(X1)) |>
  fill(monkey, .direction = 'down') |> 
  filter(!str_detect(X1, '^Monkey ')) |> 
  pivot_wider(id_cols = monkey, names_from = X1, values_from = X2) |> 
  rename_with(\(x) gsub(' ', '_', str_squish(tolower(x)))) |> 
  mutate(starting_items = str_split(starting_items, pattern = ', ')) |>
  unnest_longer(col = starting_items) |> 
  mutate(
    across(c(starting_items, test, if_true, if_false), parse_number),
    operation = str_remove(operation, pattern = "^ new = ")
  )

# table with info for each monkey
monkey_info <- data |> 
  distinct(monkey, operation, test, if_true, if_false)

# 1st problem
monkey_activity <- rep(0, length(unique(data$monkey)))

# least common denominator; used to keep the numbers manageable
lcd <- prod(unique(data$test))

# 1st and 2nd problem (just replace '%% lcd' with '/ 3' in the operation)
# 2nd problem takes ~8 minutes to run T_T
items <- set_names(data$starting_items, data$monkey)

fnc <- function(old, info){
  new <- eval(parse(text = paste('(', info$operation, ') %% lcd')))
  names(new) <- if_else((new %% info$test) == 0, info$if_true, info$if_false)
  return(new)
}

for (i in seq_len(1e4)){
  
  for(m in seq(0,7)){
    
    info <- monkey_info[monkey_info$monkey == m, ]
    
    monkey_activity[m+1] <- monkey_activity[m+1] + length(items[names(items) == m])
    
    items <- c(items[names(items) != m], fnc(items[names(items) == m], info))
    
  }
  
}

prod(sort(monkey_activity, decreasing = TRUE)[1:2])

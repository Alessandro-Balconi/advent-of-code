# load libraries
library(readr)
library(dplyr)

# read data
data <- read_delim(
  file = './2022/data/1.txt',
  delim = '\n',
  col_names = 'value',
  col_types = 'i',
  skip_empty_rows = FALSE
)

# count total number of candies for each elf
data <- data |> 
  mutate(elf = cumsum(is.na(value))) |> 
  count(elf, wt = value) 

# get maximum number of candies
data |> 
  summarise(n = max(n)) |> 
  pull()

# get sum of candies of the top 3 elves
data |> 
  slice_max(n = 3, order_by = n, with_ties = FALSE) |> 
  summarise(n = sum(n)) |> 
  pull()

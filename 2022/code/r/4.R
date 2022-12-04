# load libraries
library(tidyverse)

# read data
data <- read_csv(
  file = './2022/data/4.txt', 
  col_names = c('elf_1', 'elf_2'),
  col_types = 'cc'
)

# separate ranges into mins and maxes
data <- data |> 
  separate(col = elf_1, into = c('min_1', 'max_1'), sep = "-", convert = TRUE) |> 
  separate(col = elf_2, into = c('min_2', 'max_2'), sep = "-", convert = TRUE)

# 1st problem
data |> 
  filter((min_1 <= min_2 & max_1 >= max_2) | (min_2 <= min_1 & max_2 >= max_1)) |> 
  nrow()

# 2nd problem
data |> 
  filter((min_1 >= min_2 & min_1 <= max_2) | (min_2 >= min_1 & min_2 <= max_1))
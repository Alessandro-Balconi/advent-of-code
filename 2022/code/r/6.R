# load libraries
library(tidyverse)

# read data
data <- read_lines(file = './2022/data/6.txt')

# 1st&2nd problem
pattern_length <- 14

for (i in seq_len(nchar(data))) {
  
  unique_letters <- data |> 
    str_sub(start = i, end = i+pattern_length-1) |> 
    str_split_1(pattern = "") |> 
    unique()
  
  if(length(unique_letters) == pattern_length){ 
    cat(i+pattern_length-1)
    break
  }
  
}

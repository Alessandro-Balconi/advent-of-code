# load libraries
library(tidyverse)

# read data
data <- read_delim(
  file = './2022/data/7.txt', 
  delim = "\n", 
  col_names = 'command',
  col_types = 'c'
  )

# function that calculates the current folder path
folder_path <- function(commands){
  
  current_dir <- rep('/', length(commands))
  
  for (i in 2:length(commands)) {
    
    current_dir[i] <- if (commands[i] == '$ cd ..') { 
      
      str_remove(current_dir[i-1], '/[^/]*$') 
      
    } else if (str_detect(commands[i], pattern = "^\\$ cd ")) {
      
      file.path(current_dir[i-1], str_extract(commands[i], "(?<=^\\$ cd ).*"))
      
    } else {
      
      current_dir[i-1]
      
    }

  }
  
  return(current_dir)
  
}

# calculate folders total size (summing also the size of the ones inside it)
folder_size <- function(data, .path){
  
  data |> 
    filter(str_detect(path, .path)) |> 
    summarise(sum(size)) |> 
    pull()

}

# calculate total size of each folder
data <- data |> 
  mutate(
    path = folder_path(command),
    file_size = as.numeric(str_extract(command, '^[0-9]+'))
  ) |> 
  count(path, wt = file_size, name = 'size') %>%
  mutate(size = map_dbl(path, function(x) folder_size(data = ., .path = x)))

# 1st problem
data |> 
  filter(size <= 1e5) |> 
  summarise(sum(size)) |> 
  pull()

# 2nd problem
data |> 
  filter(size >= (3e7 - (7e7 - max(size)))) |> 
  slice_min(n = 1, order_by = size) |> 
  pull(size)

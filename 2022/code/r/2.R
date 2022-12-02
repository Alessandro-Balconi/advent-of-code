# load libraries
library(readr)
library(dplyr)

# read data
data <- read_delim(
  file = './2022/data/2.txt',
  delim = ' ',
  col_names = c('opponent', 'player'),
  col_types = 'cc'
)

# 1st problem: calculate total score; rules:
# shape score: 1 for Rock, 2 for Paper, and 3 for Scissors
# result score: 0 if you lost, 3 if the round was a draw, and 6 if you won
# opponent: A for Rock, B for Paper, and C for Scissors
# player: X for Rock, Y for Paper, and Z for Scissors
data |> 
  mutate(
    across(
      # replace letters with shape score
      .cols = c(opponent, player),
      .f = ~case_when(
        . %in% c('A', 'X') ~ 1,
        . %in% c('B', 'Y') ~ 2,
        . %in% c('C', 'Z') ~ 3
      )
    ),
    # calculate outcome of each round
    outcome = case_when(
      (player - opponent) %in% c(-1, 2) ~ 0,
      (player - opponent) == 0 ~ 3,
      (player - opponent) %in% c(1, -2) ~ 6,
    )
  ) |> 
  # get total score
  summarise(sum(player + outcome)) |> 
  pull()

# 2nd problem: calculate total score; rules:
# shape score: 1 for Rock, 2 for Paper, and 3 for Scissors
# result score: 0 if you lost, 3 if the round was a draw, and 6 if you won
# opponent: A for Rock, B for Paper, and C for Scissors
# player: X for "lose", Y for "draw" and Z for "win"
data |> 
  mutate(
    # replace opponent letters with shape score
    opponent = recode(opponent, 'A' = 1, 'B' = 2, 'C' = 3),
    # calculate outcome based on player letters
    outcome  = recode(player,   "X" = 0, "Y" = 3, "Z" = 6),
    # calculate player shape score
    player = case_when(
        outcome == 0 ~ if_else(opponent != 1, opponent - 1, 3),
        outcome == 3 ~ opponent,
        outcome == 6 ~ if_else(opponent != 3, opponent + 1, 1)
      )
    ) |> 
  # get total score
  summarise(sum(player + outcome)) |> 
  pull()

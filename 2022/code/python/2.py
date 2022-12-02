# load libraries
import pandas as pd
import numpy as np

# read data
df = pd.read_csv(
  './2022/data/2.txt', 
  sep = " ",
  names = ['opponent', 'player']
)

# 1st problem: calculate total score; rules:
# shape score: 1 for Rock, 2 for Paper, and 3 for Scissors
# result score: 0 if you lost, 3 if the round was a draw, and 6 if you won
# opponent: A for Rock, B for Paper, and C for Scissors
# player: X for Rock, Y for Paper, and Z for Scissors
df\
.assign(
  # convert letters to shape score
  **{col : np.select(
    [
      df[col].isin(['A', 'X']),
      df[col].isin(['B', 'Y']),
      df[col].isin(['C', 'Z'])
    ],
    [1, 2, 3]
    )
    for col in ['player', 'opponent']
    },
    # get result score in each round
    outcome = lambda df: np.select(
      [
        (df['player'] - df['opponent']).isin([-1, 2]),
        (df['player'] - df['opponent']) == 0,
        (df['player'] - df['opponent']).isin([1, -2])
      ],
      [0, 3, 6]
    ),
    # get round score as shape score + result score
    round_score = lambda df: df['outcome'] + df['player']
)\
.agg(total_score = ('round_score', sum))\
.iloc[0,0]

# 2nd problem: calculate total score; rules:
# shape score: 1 for Rock, 2 for Paper, and 3 for Scissors
# result score: 0 if you lost, 3 if the round was a draw, and 6 if you won
# opponent: A for Rock, B for Paper, and C for Scissors
# player: X for "lose", Y for "draw" and Z for "win"
df\
.assign(
  # replace opponent letters with shape score
  opponent = df["opponent"].replace(['A', 'B', 'C'], [1, 2, 3]),
  # calculate outcome based on player letters
  outcome = df['player'].replace(["X", "Y", "Z"], [0, 3, 6]),
  # calculate player shape score
  player = lambda df: np.select(
    [
      df['outcome'] == 0,
      df['outcome'] == 3, 
      df['outcome'] == 6
    ],
    [
      np.where(df['opponent'] != 1, df['opponent'] - 1, 3),
      df['opponent'],
      np.where(df['opponent'] != 3, df['opponent'] + 1, 1)
    ]
  ),
  # get round score as shape score + result score
  round_score = lambda df: df['outcome'] + df['player']
)\
.agg(total_score = ('round_score', sum))\
.iloc[0,0]

# load libraries
import pandas as pd
import numpy as np

# read data
df = pd.read_csv(
  './2022/data/1.txt', 
  names = ['value'],
  dtype = 'Int64',
  skip_blank_lines = False
)

# # count total number of candies for each elf
df = df\
.assign(elf = np.where(df["value"].isnull(), 1, 0).cumsum())\
.groupby("elf", as_index = False)\
.agg(value = ("value", sum))

# get maximum number of candies
df["value"].max()

# get sum of candies of the top 3 elves
df\
.nlargest(n = 3, columns = "value")\
.agg(value = ('value', sum))\
.iloc[0,0]

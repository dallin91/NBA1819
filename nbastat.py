#Practice playing around with statistics

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

nba1819 = pd.read_csv("C:\\Users\\dirk2\\Programming\\Github\\NBA1819\\1819stats.csv")

size, scale = 1000, 10

age = pd.Series('AGE'(scale, size=size) ** 1.5)

age.plot.hist(grid=True, bins=1, rwidth=0.9, color='#607c8e')
plt.title('Age of NBA Players')
plt.xlabel('Counts')
plt.ylabel('Age')
plt.grid(axis='y', alpha=0.75)

plt.show()
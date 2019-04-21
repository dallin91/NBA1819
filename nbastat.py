#Practice playing around with statistics

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns

nba1819 = pd.read_csv("C:\\Users\\dirk2\\Programming\\Github\\NBA1819\\1819stats.csv")

sns.pairplot(nba1819['AGE'])
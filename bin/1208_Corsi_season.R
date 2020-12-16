## Corsi over the course of a season
library(ggplot2)
library(data.table)

## get SAT data sorted by game (home team only)





## get basic game stats also sorted game




## can we reframe this as basic binary classification problem?
## logistic regression


## add CF% 
df_cleaned[4:12] = sapply(df_cleaned[4:12],as.numeric)
df_cleaned$corsi_for = df_cleaned$sat_for / (df_cleaned$sat_for + df_cleaned$sat_against)



## Plot 1
#y-axis = CF%
#x-axis = game
#color = win/loss or points
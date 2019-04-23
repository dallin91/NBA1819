##########################
# NBA 18-19 Analysis     #
# By: Dallin Reeves      #
# Created: 23 April 2019 #
##########################

setwd("C:/Users/dirk2/Programming/Github/NBA1819")

library(readr)
NBA <- read_csv("1819stats.csv")
attach(NBA)

hist(NBA$`MIN%Minutes PercentagePercentage of team minutes used by a player while he was on the floor`)
##########################
# NBA 18-19 Analysis     #
# By: Dallin Reeves      #
# Created: 23 April 2019 #
##########################

setwd("~/Documents/Github/NBA1819")

library(readr)
NBA <- read_csv("1819stats.csv")

library(ggplot2)
library(stargazer)
require(stats)

# Omitting rows with missing values
complete.cases(NBA)
realNBA <- na.omit(NBA)
attach(realNBA)

#Descriptive statistics
summary(realNBA)

#Scatter plot for ortg and drtg of whole league
ggplot(realNBA, aes(ORTG, DRTG)) + geom_point(aes(color = TEAM)) + 
  scale_x_continuous("Offensive Rating", breaks = seq(20,300,30))+
  scale_y_continuous("Defensive Rating", breaks = seq(60,125,by = 10))+ 
  theme_bw() + labs(title="The Real MVP")

# linear regression for ortg
or <- lm(ORTG~AGE+MPG+FTA+PPG+RPG+APG, data=realNBA)
summary(or)
stargazer(or,type="text")
coef(lm(ORTG~AGE+MPG+FTA+PPG+RPG+APG, data=realNBA))

# linear regression for drtg
dr <- lm(DRTG~AGE+MPG+SPG+BPG+PPG+Efgp, data=realNBA)
summary(dr)
stargazer(dr,type="text")

# Creating dataset for Houston Rockets
rocketsdata <- realNBA[realNBA$TEAM=="Hou", ]

ggplot(rocketsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Houston Rockets") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Atlanta Hawks
hawksdata <- realNBA[realNBA$TEAM=="Atl", ]

ggplot(hawksdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(80,300,20))+
  scale_y_continuous("Defensive Rating", breaks = seq(75,125,5))+
  theme_bw() + labs(title="Atlanta Hawks") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Boston Celtics
celticsdata <- realNBA[realNBA$TEAM=="Bos", ]

ggplot(celticsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(100,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(90,110,5))+
  theme_bw() + labs(title="Boston Celtics") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Brooklyn Nets
netsdata <- realNBA[realNBA$TEAM=="Bro", ]

ggplot(netsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,145,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,110,5))+
  theme_bw() + labs(title="Brooklyn Nets") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Charlotte Hornets
hornetsdata <- realNBA[realNBA$TEAM=="Cha", ]

ggplot(hornetsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(45,125,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(100,120,5))+
  theme_bw() + labs(title="Charlotte Hornets") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Chicago Bulls
bullsdata <- realNBA[realNBA$TEAM=="Chi", ]

ggplot(bullsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(90,125,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(100,115,5))+
  theme_bw() + labs(title="Chicago Bulls") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Cleveland Cavaliers
cavsdata <- realNBA[realNBA$TEAM=="Cle", ]

ggplot(cavsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(50,120,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(100,120,5))+
  theme_bw() + labs(title="Cleveland Cavaliers") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Dallas Mavericks
mavsdata <- realNBA[realNBA$TEAM=="Dal", ]

ggplot(mavsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Dallas Mavericks") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Denver Nuggets
nugsdata <- realNBA[realNBA$TEAM=="Den", ]

ggplot(nugsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Denver Nuggets") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Detroit Pistons
pistdata <- realNBA[realNBA$TEAM=="Det", ]

ggplot(pistdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Detroit Pistons") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Golden State Warriors
warsdata <- realNBA[realNBA$TEAM=="Gol", ]

ggplot(warsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Golden State Warriors") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Indiana Pacers
pacedata <- realNBA[realNBA$TEAM=="Ind", ]

ggplot(pacedata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Indiana Pacers") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Los Angeles Clippers
clipsdata <- realNBA[realNBA$TEAM=="Lac", ]

ggplot(clipsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Los Angeles Clippers") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Los Angeles Lakers
lakedata <- realNBA[realNBA$TEAM=="Lal", ]

ggplot(lakedata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Los Angeles Lakers") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Memphis Grizzlies
grizdata <- realNBA[realNBA$TEAM=="Mem", ]

ggplot(grizdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Memphis Grizzlies") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Miami Heat
heatdata <- realNBA[realNBA$TEAM=="Mia", ]

ggplot(heatdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Miami Heat") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Milwaukee Bucks
buckdata <- realNBA[realNBA$TEAM=="Mil", ]

ggplot(buckdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Milwaukee Bucks") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Minnesota Timberwolves
wolvesdata <- realNBA[realNBA$TEAM=="Min", ]

ggplot(wolvesdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Minnesota Timberwolves") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# New Orleans Pelicans
pelsdats <- realNBA[realNBA$TEAM=="Nor", ]

ggplot(pelsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="New Orleans Pelicans") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# New York Knicks
knickdata <- realNBA[realNBA$TEAM=="Nyk", ]

ggplot(knickdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="New York Knicks") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Oklahoma City Thunder
okcdata <- realNBA[realNBA$TEAM=="Okc", ]

ggplot(okcdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Oklahoma City Thunder") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Orlando Magic
magdata <- realNBA[realNBA$TEAM=="Orl", ]

ggplot(orldata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Orlando Magic") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Philadelphia 76ers
phidata <- realNBA[realNBA$TEAM=="Phi", ]

ggplot(phidata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Philadelphia 76ers") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Phoenix Suns
sunsdata <- realNBA[realNBA$TEAM=="Pho", ]

ggplot(sunsdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Phoenix Suns") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Portland Trailblazers
pordata <- realNBA[realNBA$TEAM=="Por", ]

ggplot(pordata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Portland Trailblazers") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Sacramento Kings
kingdata <- realNBA[realNBA$TEAM=="Sac", ]

ggplot(kingdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Sacramenta Kings") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# San Antonio Spurs
spurdata <- realNBA[realNBA$TEAM=="San", ]

ggplot(spurdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="San Antonio Spurs") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Toronto Raptors
rapdata <- realNBA[realNBA$TEAM=="Tor", ]

ggplot(rapdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Toronto Raptors") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Utah Jazz
jazzdata <- realNBA[realNBA$TEAM=="Uta", ]

ggplot(jazzdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Utah Jazz") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)

# Washington Wizards
wizdata <- realNBA[realNBA$TEAM=="Was", ]

ggplot(wizdata, aes(ORTG, DRTG)) + geom_point(aes(color = `NAME	`)) +
  scale_x_continuous("Offensive Rating", breaks = seq(75,135,5))+
  scale_y_continuous("Defensive Rating", breaks = seq(85,120,5))+
  theme_bw() + labs(title="Washington Wizards") + geom_vline(xintercept = 108)+
  geom_hline(yintercept = 104)
library(tidyverse)
library(ggpubr)
SP500 = read.csv("SP500.csv")
rSP500 = SP500$Log.return
#### a ####
#the probability that on a randomly chosen day the SP500 index is down
ndays = length(rSP500)
pro1 = sum(rSP500 < 0)/ndays

#### b ####
#the probability that the SP500 is down given it was down the previous day
x1 = sum(rSP500[1:(ndays-1)]<0 & rSP500[2:ndays]<0)
x2 = sum(rSP500[1:(ndays-1)]<0)
pro2 = x1/x2

#### c ####
#the probability that the absolute value of the log-returns of the SP500 on a randomly selected day is at least 1.5%
SP500$Log.return = as.numeric(SP500$Log.return)
subdf = subset(SP500,abs(SP500$Log.return) >= 0.015)
pro3 = length(subdf)/ndays

#### d ####
# B = the absolute value of the log-returns of the SP500 is at least 1% on a randomly selected day
subdf1 = subset(SP500,abs(SP500$Log.return) >= 0.01)
happen_B = length(subdf1)
#the probability that A = the absolute value of the log-return of the following day is at least 1.5%
happen_AB = sum(abs(subdf1$Log.return[1:(length(subdf1)-1)]) >= 0.01 & abs(subdf1$Log.return[2:length(subdf1)]) >= 0.015)
pro4 = happen_AB/happen_B
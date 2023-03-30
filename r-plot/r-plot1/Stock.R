library(tidyverse)
library(ggpubr)
APPL = read.csv("data/APPL.csv")
IBM = read.csv("data/IBM.csv")
JNJ = read.csv("data/JNJ.csv")
SP500 = read.csv("data/SP500.csv")

#Plot the time series of the adjusted closing prices of all four assets in a single pane.
APPL$Date = as.Date(APPL$Date)
p1 = ggplot(APPL) +
  geom_line(aes(x=Date, y=Adj.Close)) + 
  labs(title = "APPL", x = "Time", y = "Adjusted closing prices")

IBM$Date = as.Date(IBM$Date)
p2 = ggplot(IBM) +
  geom_line(aes(x=Date, y=Adj.Close)) + 
  labs(title = "IBM", x = "Time", y = "Adjusted closing prices")

JNJ$Date = as.Date(JNJ$Date)
p3 = ggplot(JNJ) +
  geom_line(aes(x=Date, y=Adj.Close)) + 
  labs(title = "JNJ", x = "Time", y = "Adjusted closing prices")

SP500$Date = as.Date(SP500$Date)
p4 = ggplot(SP500) +
  geom_line(aes(x=Date, y=Adj.Close)) + 
  labs(title = "SP500", x = "Time", y = "Adjusted closing prices")

ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2)



# Compare the distributions of the log-returns of all four assets using a side-by-side boxplot.
p5 = ggplot(APPL) +
  geom_boxplot(mapping = aes(y = Log.return)) + 
  labs(title = "APPL", y = "Log-returns")

p6 = ggplot(IBM) +
  geom_boxplot(aes(y=Log.return)) + 
  labs(title = "IBM", y = "Log-returns")

p7 = ggplot(JNJ) +
  geom_boxplot(aes(y=Log.return)) + 
  labs(title = "JNJ", y = "Log-returns")

SP500$Log.return = as.numeric(SP500$Log.return)
p8 = ggplot(SP500) +
  geom_boxplot(aes(y=Log.return)) + 
  labs(title = "SP500", y = "Log-returns")

ggarrange(p5,p6,p7,p8, ncol = 4, nrow = 1)

# c
log = data.frame(APPL$Log.return, IBM$Log.return, JNJ$Log.return, SP500$Log.return)
summary(log)


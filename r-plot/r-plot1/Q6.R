library(tidyverse)
library(ggpubr)
APPL = read.csv("data/APPL.csv")
JNJ = read.csv("data/JNJ.csv")

#### a ####
ggplot(APPL) +
  geom_histogram(aes(x=Log.return),bins = 80) + 
  labs(title = "Distribution of APPL log return")

ggplot(JNJ) +
  geom_histogram(aes(x=Log.return),bins = 80) + 
  labs(title = "Distribution of JNJ log return")

#### b ####
#t
t = curve(dt(x,df=4),from=-4, to=4)

#### c ####
x1 = JNJ[c(2063:1812),]
p1 = ggplot(x1) +
  geom_boxplot(mapping = aes(y = Log.return)) + 
  labs(title = "JNJ during the 2008 financial crisis", y = "Log-returns")
x2 = JNJ[c(1306:1),]
p2 = ggplot(x2) +
  geom_boxplot(mapping = aes(y = Log.return)) + 
  labs(title = "JNJ after the 2008 financial crisis")
ggarrange(p1,p2, ncol = 2, nrow = 1)

p3 = ggplot(x1) +
  geom_histogram(mapping = aes(y = Log.return)) + 
  labs(title = "JNJ during the 2008 financial crisis", y = "Log-returns")
p4 = ggplot(x2) +
  geom_histogram(mapping = aes(y = Log.return)) + 
  labs(title = "JNJ after the 2008 financial crisis", y = "Log-returns")
ggarrange(p3,p4, ncol = 2, nrow = 1)

qqnorm(x1$Log.return)
qqnorm(x2$Log.return)


#### d ####
x3 = APPL[c(1306:1),]
ggplot(x3,aes(sample=Log.return))+
  geom_qq()+geom_qq_line()

ggplot(x3,aes(x=Log.return),bins = 50)+
  geom_histogram(position = "identity")


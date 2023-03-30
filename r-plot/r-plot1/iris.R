library(tidyverse)
df=iris

#### a ####
#Draw the histogram of the distribution of the petal length of the iris flower
ggplot (data = df) +
  geom_histogram(mapping = aes(x=Petal.Length), breaks=seq(0,8,l=17)) +
  labs (title="Distribution of Petal Length", x="Petal Length", y = "Iris Count")

#Distribution
shapiro.test(df$Petal.Length) #p>0.05, normal distribution

#The iris flowers in the sample have a petal length <= 2cm
subdf = subset(df,df$Petal.Length <= 2)

#### b ####
#Draw individual histograms of the distribution of petal length for each plant species
ggplot (data = df) +
  geom_histogram(mapping = aes(x = Petal.Length), breaks=seq(0,8,l=17)) +
  labs (title="Distribution of Petal Length", x="Petal Length", y = "Iris Count")+
  facet_wrap(~Species,scales = "free")

#-------------------------分割线----------------------------
#the most striking difference between the three distributions
subdf1 = subset(df,df$Species == "setosa")
subdf1$Species = droplevels(subdf1$Species)
table(subdf1$Species)
subdf2 = subset(df,df$Species == "versicolor")
subdf2$Species = droplevels(subdf2$Species)
table(subdf2$Species)


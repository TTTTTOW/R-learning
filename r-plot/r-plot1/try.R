2+2
x=1
y <- 2

install.packages("tidyverse")
library(tidyverse)
?datasets
help("datasets")

iris
df = iris
head(df)
tail(df)
summary(df)
str(df)

sd = read.csv("data/college.csv")
sd = read.csv("C:/Users/Wild/Desktop/r-plot")

sd$city = as.factor(sd$city)
sd = mutate(sd, region=as.factor(region),
            highest_degree=as.factor(highest_degree))

ggplot(data = sd)+
  geom_bar(mapping = aes(x = region, fill = region))+
  labs(title="Distribution",fill="region",x="Region",y="Student Count")

ggplot(data = sd)+
  geom_histogram(mapping = aes(x=sat_avg))+
  labs(title="Distribution",x="Sat scores",y="Student Count")

ggplot(data = sd)+
  geom_point(mapping = aes(x=sat_avg,y=loan_default_rate))+
  labs(title="relationship",x="Sat scores",y="loan default rate")
  


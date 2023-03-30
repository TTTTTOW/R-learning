
attach(Q1.df)
#### a ####
#Create a boxplot and a histogram of the test times for the year 7 
boxplot(Year7,main="Boxplot of year 7 students")
hist(Year7,main="Histogram of year 7 students",xlab="Test times")
# looks like normal distribution



#### b ####
#calculate the sample range, the sample interquartile range and the sample coefficient of variation of the test times for the year 7 students.
IQR(Year7) #interquartile range
max(Year7)-min(Year7) #sample range
sd(Year7)/mean(Year7) # coefficient of variation



#### c ####
#shorter than 43.75 minutes is considered “great”, a test time between 43.75 and 45.15 minutes is considered “good”, between 45.15 and 48.95 minutes is considered “average”, a test time between 48.95 and 54.25 minutes is considered “mediocre” and a test time longer than 54.25 minutes is considered “poor”
Q1.df$group[Year7<= 43.75]="great"
Q1.df$group[Year7 > 43.75 & Year7 <= 45.15] = "good"
Q1.df$group[Year7 > 45.15 & Year7 <= 48.95] = "average"
Q1.df$group[Year7 > 48.95 & Year7 <= 54.25] = "mediocre"
Q1.df$group[Year7 > 54.25] = "poor"
head(mathScore)
counts = table(group)
barplot(counts, main="Test performance for the year 7 students",xlab="Test performance", ylab="Number of students")

#### d ####
# complete the test in less than 50.55 minutes,  α = 5%
## H_0: p>=0.415, H_α: p<0.415
x = Year7[Year7 <= 50.55]
n = length(Year7)
p0 = length(x)/n
zd = (p0-0.415)/sqrt(0.415*(1-0.415)/n)
pd = pnorm(zd,lower.tail=TRUE)



#### e ####
#Test whether the population proportion of year 7 students within the school district that would take longer than 50.75 minutes to complete the test 
longer7 = Year7[Year7 > 50.75]
p1 = length(longer7)/n
#is less than the population proportion of year 8 students within the school district that would complete the test in less than 49.55 minutes. 
less8 = Year8[Year8 < 49.55]
n2 = length(Year8)
p2 = length(less8)/n2
p_bare = (n*p1+n2*p2)/(n2+n)
ze = (p1-p2)/sqrt(p_bare*(1-p_bare)*(1/250))
pe = pnorm(ze,lower.tail=TRUE)



#### f ####
x= Year7[1:6]
x1=x[6]-x[1]
x2=x[5]-x[1]
x3=x[4]-x[1]
x4=x[3]-x[1]
x5=x[6]-x[2]
x6=x[5]-x[2]
x7=x[4]-x[2]
x8=x[6]-x[3]
x9=x[5]-x[3]
x10=x[6]-x[4]
y=c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)


#### g ####
sub0 = y[y>5 & y<8]
sub1 = y[y>5]
p_g = length(sub0)/length(sub1)


#### h ####
var(y)





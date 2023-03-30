
attach(Q2.df)
#### a ####
#Test whether the population variance of body weight is the same for people who play football and for people who play tennis, significance level of α = 1%.
football = Weight[Sport == "Football"]
var_foot = var(football)
tennis = Weight[Sport == "Tennis"]
var_tennis = var(tennis)
F_a = var_foot/var_tennis
df_foot = length(football)-1
df_tennis = length(tennis)-1
p_a = pf(F_a, df_foot, df_tennis, lower.tail = FALSE)
#check
var.test(football, tennis)



#### b ####
# Test whether the population mean body weight of people who play tennis is greater than the population mean body weight of people who play football by more than 0.9 kilograms, use a significance level of α = 1%.
mean_foot = mean(football)
mean_tennis = mean(tennis)
t_b = (mean_tennis-mean_foot-0.9)/sqrt(var_foot/length(football)+var_tennis/length(tennis))
p_b = pt(t_b,df_foot+df_tennis)



#### c ####
# Use a single test to test whether the population mean body weight is the same across all recreational sports, α = 2%.
volleyball = Weight[Sport == "Volleyball"]
mean_volley = mean(volleyball)
var_volley = var(volleyball)
sst = var(Weight)*(300-1)
sse = var_foot*(length(football)-1)+var_tennis*(length(tennis)-1)+var_volley*(length(volleyball)-1)
sstr = sst-sse
f_c = sstr/2/(sse/(300-3))
qf(0.98,2,300-3)


#### d ####
#Each population has to be normally distributed;
hist(football, main="Football")
hist(tennis, main="Tennis")
hist(volleyball, main="Volleyball")
#Each population has the same variance;
var_foot
var_tennis
var_volley
#The observations in each sample are independent of each other;


#### e ####
# Test whether the population mean body weight of right-handed people who play tennis is less than 86.5 kilograms. Clearly state your hypotheses, making sure to define any parameters, and use a significance level of α = 0.5%.
right = Weight[Hand=="Right" & Sport == "Tennis"]
right_bar = mean(right)
std = sd(right)
t_e = (right_bar-86.5)/(std/sqrt(length(right)))
qt(0.005,length(right)-1)


#### f ####
#Create a scatter plot of bench press amount against body weight. Make sure to give your plot an appropriate title and appropriate labels for the x and y axes. Describe the relationship between these two variables.
plot(Weight, BenchPress,main="Scatter plot of BenchPress against Weight",xlab="Weight",ylab="BenchPress")


#### g ####
#  Test whether the correlation between bench press amount and body weight is greater than zero. Clearly state your hypotheses, making sure to define any parameters, and use a significance level of α = 5%.
r_g = cor(Weight,BenchPress)
t_g = r_g*sqrt(300-2)/sqrt(1-r_g^2)
qt(0.95,length(Weight)-2)

#### h ####
#  Fit a simple linear regression model with bench press amount as the dependent variable and body weight as the independent variable. Write down the estimated regression model.
model_h = lm(BenchPress~Weight)


#### i ####
#  Discuss whether the assumptions for a simple linear regression model hold for the model you fitted in part (h), making sure to provide clear justifications for your answer.
## 回归分析的基本假设


#### j ####
# Considering only left-handed people, fit a simple linear regression model with bench press amount as the dependent variable and body weight as the independent variable. 
y_j = Weight[Hand=="Left"]
x_j = BenchPress[Hand=="Left"]
b_j1 = cov(x_j,y_j)/var(x_j)
b_j0 = mean(y_j)-b_j1*mean(x_j)
# Use the estimated regression model to predict the bench press amount for a left-handed person who weighs 80 kilograms without using any R functions that are designed to calculate any predictions.
estimated = b_j0+b_j1*80


#### k ####
# For right-handed people who weigh more than 87.5 kilograms, test whether the population mean body weight is greater than the population mean bench press amount by more than 13.5 kilograms. α = 2.5%.
k = Weight[Hand=="Right"&Weight>87.5]-BenchPress[Hand=="Right"&Weight>87.5]
x_bark = mean(k)
std_k = sd(k)
t_k = (x_bark-13.5)/(std_k/sqrt(length(k)))
qt(0.975,length(k)-1)





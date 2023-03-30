library(plm)
df = read.csv("D:/#Desktop/D/R-10.24/panelData.csv")
rankData = pdata.frame(df,index=c("省市","Year"))
data2022 = read.csv("D:/#Desktop/D/R-10.24/data2022.csv")
data2022 = pdata.frame(data2022,index=c("省市","Year"))


#pool
pool = plm(土地均价~ 企业拿地占比,data=rankData,model="pooling")
summary(pool)
# Adj. R-Squared: 0.17413

#OSL
ols = lm(土地均价~ 企业拿地占比,data=rankData)
summary(ols)
# Adjusted R-squared:  0.1741


## 个体固定效应的Panel模型
#LSDV模型
LSDV = lm(土地均价~ 企业拿地占比+factor(省市)-1,data = df)
summary(LSDV)
# Adjusted R-squared:  0.8826
# 这个Adjusted R-squared最大，是最好的
predict(LSDV, newdata=data2022)

#组内模型
within <- plm(土地均价~ 企业拿地占比, data=rankData, effect = "individual",model="within")
summary(within) 
# Adj. R-Squared: 0.83564
fixef(within)
# 偏离总体截距的情况
summary(fixef(within))
predict(within, newdata=data2022)


## 时间固定效应Panel模型
within2 = plm(土地均价~ 企业拿地占比,data=rankData,effect = "time",model="within")
summary(within2)
# Adj. R-Squared: 0.095392


## 个体和时间双维固定效应模型
within3 <- plm(土地均价~ 企业拿地占比,data=rankData,effect = "twoways",model="within")
summary(within3)
# Adj. R-Squared: 0.82295


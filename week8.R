setwd("//home.org.aalto.fi/savelaj3/data/Documents/R/Intr_to_stat_infer") #koulun kone

gdp <- read.table("E8GDP.txt", header = T)
gdp

#### 2 ####

y <- gdp$GDP.growth
x <- gdp$Private.consumption.growth

model <- lm(y~x)
summary(model)
# b_0 = -1.049
# b_1 = 1.906

cor(y, model$fitted.values)^2 # R^2 = 0.22461...

#### 3 ####

plot(y~x)
abline(model)
with(gdp, text(y~x, labels=gdp$Country,pos=1))

res <- model$residuals
fit <- model$fitted.values

plot(x, res)
abline(h=0)

plot(y, fit)
abline(a=0, b=1)

#### 4 ####

gdp2 <- subset(gdp, !gdp$Country=="Ireland")

y2 <- gdp$GDP.growth[-length(gdp$Country)]
x2 <- gdp$Private.consumption.growth[-length(gdp$Country)]

model2 <- lm(y2~x2)

summary(model2)

plot(y2~x2)
abline(model2)




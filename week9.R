setwd("//home.org.aalto.fi/savelaj3/data/Documents/R/Intr_to_stat_infer") #koulun kone

list.files()

gdp <- read.table("E9GDP.txt", header = T)
gdp

y <- gdp$GDP.growth
x <- gdp$Private.consumption.growth

model <- lm(y~x)
summary(model)

#### 2 ####

confint.lm(model, level = 0.9)

#### 3 ####


bootstrap <- function(x, y, n, alpha) {
  model <- lm(y~x)
  
  b0 <- model$coefficients[1]
  b1 <- model$coefficients[2]
  r2 <- cor(y, model$fitted.values)^2
  
  for (i in 1:n) {
    y2 <- model$fitted.values + sample(model$residuals, replace = T)
    model2 <- lm(y2~x)
    b0 <- c(b0, model2$coefficients[1])
    b1 <- c(b1, model2$coefficients[2])
    r2 <- c(r2, cor(y2, model2$fitted.values)^2)
  }
  
  b0.sorted <- sort(b0)
  b1.sorted <- sort(b1)
  r2.sorted <- sort(r2)
  b0_left <- b0.sorted[round((n+1)*alpha/2)]
  b1_left <- b1.sorted[round((n+1)*alpha/2)]
  r2_left <- r2.sorted[round((n+1)*alpha/2)]
  b0_right <- b0.sorted[round((n+1)*(1-alpha/2))]
  b1_right <- b1.sorted[round((n+1)*(1-alpha/2))]
  r2_right <- r2.sorted[round((n+1)*(1-alpha/2))]
  result <- c(b0_left, b0_right, b1_left, b1_right, r2_left, r2_right)
  return(result)
}

ci1 <- bootstrap(x, y, 999, 0.1)
ci1
summary(model)

qqnorm(model$residuals)
shapiro.test(model$residuals)

#### 4 ####

gdp2 <- subset(gdp, !gdp$Country=="Ireland")

x2 <- gdp2$Private.consumption.growth
y2 <- gdp2$GDP.growth

model2 <- lm(y2~x2)

ci2 <- bootstrap(x2, y2, 999, 0.1)
ci2
summary(model2)

qqnorm(model2$residuals)
shapiro.test(model2$residuals)


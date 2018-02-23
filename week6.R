setwd("//home.org.aalto.fi/savelaj3/data/Documents/R/Intr_to_stat_infer") #koulun kone


#### 2 ####

a1 <- rnorm(50)
a2 <- rchisq(50, 3)
a3 <- runif(50)
a4 <- rnorm(50)
a4[1] = -15

# a)

par(mfrow=c(2,2))
hist(a1)
hist(a2)
hist(a3)
hist(a4)
par(mfrow=c(1,1))

boxplot(a1)
boxplot(a2)
boxplot(a3)
boxplot(a4)


# b)

mean(a1)
mean(a2)
mean(a3)
mean(a4)

median(a1)
median(a2)
median(a3)
median(a4)

library(e1071)

skewness(a1)
skewness(a2)
skewness(a3)
skewness(a4)

kurtosis(a1)
kurtosis(a2)
kurtosis(a3)
kurtosis(a4)

apply(data.frame(a1,a2,a3,a4),2,mean)
apply(data.frame(a1,a2,a3,a4),2,median)
apply(data.frame(a1,a2,a3,a4),2,skewness)
apply(data.frame(a1,a2,a3,a4),2,kurtosis)


# c)

BS <- function(x) {
  library(e1071)
  n <- length(x)
  n * (skewness(x)^2/6+kurtosis(x)^2/24)
}

apply(data.frame(a1, a2, a3, a4), 2, BS)


# d)

par(mfrow=c(2,2))
qqnorm(a1, main = "Q-Q Plot a1")
qqline(a1, col=2)
qqnorm(a2, main = "Q-Q Plot a2")
qqline(a2, col=2)
qqnorm(a3, main = "Q-Q Plot a3")
qqline(a3, col=2)
qqnorm(a4, main = "Q-Q Plot a4")
qqline(a4, col=2)
par(mfrow=c(1,1))

shapiro.test(a1)
shapiro.test(a2)
shapiro.test(a3)
shapiro.test(a4)


#### 3 ####


# H0: the dice is fair (p_i = 1/6)
# H1: the dice is not fair
# alpha = 3%

x <- c(12, 16, 20, 17, 22, 33)
p <- rep(1/6,6)

barplot(x)

chisq.test(x)
chisq.test(x,p=p)


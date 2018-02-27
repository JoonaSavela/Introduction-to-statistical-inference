setwd("//home.org.aalto.fi/savelaj3/data/Documents/R/Intr_to_stat_infer") #koulun kone


hs1 <- read.table("H01HEIGHTSHOE.txt", header = T)
hs7 <- read.table("H07HEIGHTSHOE.txt", header = T)

cor(hs7)
cor(hs7, method = "spearman")

hs <- rbind(hs1, hs7)
hs

plot(hs$shoesize, hs$height) # cor is around 0.8

cor(hs$height, hs$shoesize)
cor(hs$height, hs$shoesize, method = "spearman")


#H0: p(x, y) = 0
#H1: p(x, y) != 0
#alpha = 0.05

# method 1
x <- hs$shoesize
y <- hs$height

t <- cor(x, y)

z <- 0

for (i in 1:50000) {
  z[i] <- cor(x, sample(y))
}

z <- sort(z)

hist(z)

zz <- 0

for (i in 1:length(z)) {
  if (abs(z[i]) > t) {
    zz <- zz + 1
  }
}

p.value <- zz / length(z)

# method 2
cor.test(x, y)


# method 3

library(combinat)

x <- x[1:10]
y <- y[1:10]
t <- cor(x, y)
permCor <- sapply(permn(x), y = y, method = "pearson", cor)

z <- permCor

hist(z)

zz <- 0

for (i in 1:length(z)) {
  if (abs(z[i]) > t) {
    zz <- zz + 1
  }
}

p.value <- zz / length(z)




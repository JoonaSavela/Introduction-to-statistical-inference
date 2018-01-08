setwd("//home.org.aalto.fi/savelaj3/data/Documents/R/Intr_to_stat_infer") #koulun kone

#### 2 ####

v = rnorm(100)

hist(v)

mean(v)
sd(v)

# try data.frame(b,c,d), where b, c, and d are generated samples

a = matrix(NA, nrow = 20, ncol = 3)

for (i in 1:3) {
  a[,i] = rnorm(20, mean = 1, sd = sqrt(3))
}

m = c()
s = c()

for (i in 1:3) {
  hist(a[,i])
  m[i] = mean(a[,i])
  s[i] = sd(a[,i])^2
}

m
s


#### 3 ####


heights = c(178,170,178,158,165,173,183,187,174,177,168,175,190,181)

hist(heights)


eyeColors = c(1,2,3,4,3,3,5,3,3,2,2,5,2,2,1)

eyetable = table(eyeColors)

freq = unname(eyetable)

colors = c("gray", "blue", "green", "black", "brown")

pie(eyetable, labels = colors, col = colors, main = "Eye colors of participans")


setwd("//home.org.aalto.fi/savelaj3/data/Documents/R/Intr_to_stat_infer") #koulun kone

#### 2 ####

compr <- read.csv("E3COMPR.txt", header = T, sep = "\t")

# a)

m1 <- mean(compr$CONCR1, na.rm = T)
m2 <- mean(compr$CONCR2)
colMeans(compr, na.rm = T)

s1 <- sd(compr$CONCR1, na.rm = T)
s2 <- sd(compr$CONCR2)

diag(sqrt(var(compr, na.rm = T)))

par(mfrow=c(1,2))
hist(compr$CONCR1, breaks = 12)
hist(compr$CONCR2, breaks = 8)

# b)

# null hypothesis: mu1 = mu2
# alternative hypothesis: mu1 != mu2

t.test(compr$CONCR1, compr$CONCR2, alternative = "t", paired = F)


# c)

# - i.i.d
# - normal distribution (normality)


#### 3 ####

x <- read.csv("E3SALARY.txt", header = T, sep = "\t")

# a)

men.mean = mean(x$MALE)
men.mean

women.mean = mean(x$FEMALE)
women.mean

men.std = sd(x$MALE)
men.std

women.std = sd(x$FEMALE)
women.std

men.min = min(x$MALE)
men.min

women.min = min(x$FEMALE)
women.min

men.max = max(x$MALE)
men.max

women.max = max(x$FEMALE)
women.max

colMeans(x[,1:2])
diag(sqrt(var(x[,1:2])))
range(x[,1])
range(x[,2])


bootstrap <- function(a) {
  a.mean <- mean(a)
  n <- 999
  alpha <- 0.1
  
  a.b <- a.mean
  
  for (i in 1:n) {
    b <- sample(a, replace = T)
    a.b <- c(a.b, mean(b))
  }
  
  a.b.sorted <- sort(a.b)
  left <- a.b.sorted[round((n+1)*alpha/2)]
  right <- a.b.sorted[round((n+1)*(1-alpha/2))]
  return(c(left, right))
}

# hist(bootstrap(x$MALE))

men.confint <- bootstrap(x$MALE)
men.confint

women.confint <- bootstrap(x$FEMALE)
women.confint

boxplot(x[,1:2], col = "cyan")

# b)

# null hypothesis: mu_m = mu_f
# alternative hypothesis: mu_m != mu_f

t.test(x$MALE, x$FEMALE, paired = T, alternative = "t")

# c)

# null hypothesis: mu_m = mu_f
# alternative hypothesis: mu_m != mu_f

t.test(x$MALE, x$FEMALE, paired = F, alternative = "t")


# d)

# the observations are paired

# iid
# normality

# the paired test is more reliable




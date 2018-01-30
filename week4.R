setwd("//home.org.aalto.fi/savelaj3/data/Documents/R/Intr_to_stat_infer") #koulun kone

bootstrap <- function(a) {
  a.median <- median(a)
  n <- 999
  alpha <- 0.1
  
  a.b <- a.median
  
  for (i in 1:n) {
    b <- sample(a, replace = T)
    a.b <- c(a.b, median(b))
  }
  
  a.b.sorted <- sort(a.b)
  left <- a.b.sorted[round((n+1)*alpha/2)]
  right <- a.b.sorted[round((n+1)*(1-alpha/2))]
  return(c(left, right))
}

library(boot)
library(BSDA)

#### 2 ####

q <- read.csv("E4QUEUE.txt", sep = "\t", na.strings = "M")
q$QUEUEB

# a)

qa <- q$QUEUEA
qb <- na.omit(q$QUEUEB)

median(qa)
median(qb, na.rm = T)
bootstrap(qa) # (6.8, 8.5)
bootstrap(qb) # (8.1, 9.9)

sample.median <- function(x, d) return(median(x[d]))

boota <- boot(qa, sample.median, R=1000, stype = "i")
boot.ci(boota, conf = 0.9) # (6.8, 8.5)

bootb <- boot(qb, sample.median, R=1000, stype = "i")
boot.ci(bootb, conf = 0.9) # (8.1, 9.9)

sample.mad <- function(x, d) return(mad(x[d]))

mad(qa)
bootamad <- boot(qa, sample.mad, R=1000, stype = "i")
boot.ci(bootamad, conf = 0.9) # (0.445, 2.076)

mad(qb)
bootbmad <- boot(qb, sample.mad, R=1000, stype = "i")
boot.ci(bootbmad, conf = 0.9) # (0.297, 1.927)

par(mfrow=c(1,2))
hist(qa, main = "A", xlim = c(5, 12))
hist(qb, main = "B", xlim = c(5, 12))
par(mfrow=c(1,1))

# b)

# null: m_a = m_b
# alternative: m_a != m_b

wilcox.test(qa, qb, alternative = "t")
# t.test(qa, qb, alternative = "t")

# c)

# iid
# symmetric distributions
# distributions need to be the same apart from a possible locaton shift

#### 3 ####

s <- read.csv("E4SALARY.txt", sep = "\t")
s

# a)

sm <- s$MALE
sw <- s$FEMALE

median(sm)
median(sw)

mad(sm)
mad(sw)

range(sm)
range(sw)

plot(sw, sm, xlab = "Salary, women", ylab = "Salary, men")
abline(a = 0, b = 1, col = "red")


# b)

# null: m_m = m_w
# alternative: m_m != m_w

SIGN.test(sw, sm) # p-value = 0.2891 > 0.05

wilcox.test(sw, sm, alternative = "t", paired = T) # p-value = 0.05469 > 0.05

# => salaries are equal(?)

t.test(sw, sm, alternative = "t", paired = T) # p-value = 0.03738 < 0.05

# => salaries are not equal(?)

# c)

# Paired sign test:
# iid; pared values can be dependent

# Paired Wilcoxon signed rank test: 
# iid; pared values can be dependent
# symmetric distributions

par(mfrow=c(1,2))
hist(sw)
hist(sm)
par(mfrow=c(1,1))

# paired sign test is perhaps the most appropriate, but also paired wilcoxon rank test could be used

wilcox.test(sw-sm, alternative = "t")


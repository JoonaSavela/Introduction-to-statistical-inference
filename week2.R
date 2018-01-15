setwd("//home.org.aalto.fi/savelaj3/data/Documents/R/Intr_to_stat_infer") #koulun kone

#### 2 ####

POINTS = c(6, 0, 0, 2, 1, 0, 5, 5, 0, 6, 5, 0, 1, 5, 6, 5, 1, 6, 6, 0)

# a)

freq <- table(factor(POINTS, levels = 0:6))

barplot(freq)

# pointsTable = table(POINTS)

# barplot(pointsTable)

# b)

p_mean = mean(POINTS)

p_std = sd(POINTS)

p_var = p_std^2

p_min = min(POINTS)

p_max = max(POINTS)

p_range = range(POINTS)

p_median = median(POINTS)

p_mode = as.numeric(names(freq[which.max(unname(freq))]))


#### 3 ####


suntan <- read.table("E2SUNTAN.txt", header = T)
# View(suntan)

# a)

range(suntan)

sun.breaks = seq(range(suntan)[1]-1, range(suntan)[2]+1, by=2)

suntan.cut = cut(suntan[,1], sun.breaks, right=T)
suntan.cut

barplot(table(suntan.cut))

hist(suntan[,1], breaks = sun.breaks)

# b)

suntan.table = table(suntan)

suntan.mean = mean(suntan[,1])
suntan.var = var(suntan[,1])
suntan.std = sd(suntan[,1])
suntan.min = min(suntan[,1])
suntan.max = max(suntan[,1])
suntan.median = median(suntan[,1])
suntan.mad = mad(suntan[,1])
suntan.mode = as.numeric(names(suntan.table[which.max(unname(suntan.table))]))

suntan.skew = 1/length(suntan[,1]) * sum((suntan[,1] - suntan.mean)^3) / suntan.std^3

suntan.kur = 1/length(suntan[,1]) * sum((suntan[,1] - suntan.mean)^4) / suntan.std^4

# c)

boxplot(suntan)


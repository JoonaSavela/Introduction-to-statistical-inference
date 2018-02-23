# BIASED DICE, © Kévin Selänne
#
# The following simulation demonstrates how many observations are needed to
# detect a biased die using the chi-squared goodness-of-fit test. The biased,
# generalized die needs to have at least two sides (in which case it could be
# described as a coin).
#
# First, the die is biased by adding probability to the first side and evenly
# substracting that amount from the remaining sides. Then, a fixed number of
# samples is drawn from the die, and the resulting sample is tested for equal
# distribution with the goodness-of-fit test. If the test's p-value exceeds the
# chosen significance level, more samples are drawn and concatenated to previous
# observations until a statistically significant result is obtained. A vector
# saves this number of observations required to detect the bias, and the process
# is repeated for another biased die. Finally, a plot shows the number of
# observations that were needed to detect bias as a function of probability
# added to the first side of the die.
#
# A real-world illustration could be a casino offering dice games. A malicious
# dice manufacturer could sell a biased die to a casino, and then go play games
# at that casino betting on side 1 of his die. He would then have a slightly
# higher chance of winning compared to playing with a fair die. The casino owner
# keeps track of all dice throws and regularly uses the chi-squared
# goodness-of-fit test to ensure that all dice are fair. According to this
# simulation, the smaller the bias, the more throws it would take for the casino
# owner to notice that the die is biased. On the other hand, general probability
# theory tells us that the malicious manufacturer would only benefit from the
# biased die in the long term, when sample averages converge to their expected
# values (see law of large numbers). For example, if the malicious manufacturer
# were to play just 10 games, he would most likely not benefit from the biased
# die, since such a small sample cannot account for a tiny increase in the
# probability of one side. The benefits would only be noticeable after thousands
# of games, at which point the casino owner would have detected the hoax.
# (Though the seller could circumvent this limitation by playing in multiple
# casinos, satisfying the requirement of many games while playing few games in
# each casino to avoid getting caught.)

alpha <- 0.001                    # Significance level
sides <- 6                        # How many sides the die has (>=2)
die <- seq(1,sides)               # Numbering the sides
base.probs <- rep(1/sides, sides) # Fair, uniform probabilities
# Values of bias that will be added to the first side:
k <- seq((sides*12), (sides*3), length.out = 150)
bias <- 1/k                       # denser scale at small values of bias

final.n <- rep(NA, length(bias))  # Initializing a vector for the number of observations finally needed
for(b in 1:length(bias)) {
  # Setting up the probabilities vector to be used in the upcoming iteration:
  biased.probs <- rep(NA, sides)
  # Adding bias to the first side:
  biased.probs[1] <- base.probs[1] + bias[b]
  # Normalization by distributing the remaining probability evenly:
  biased.probs[2:sides] <- base.probs[2:sides] - bias[b]/(sides-1)
  
  # Initializing an empty frequency table:
  observations.freq <- table(factor(numeric(0), levels = 1:sides))
  delta.n <- 30 # How many observations to generate at each increment
  a <- 1        # Initial value for the "observed" p-value
  while(a > alpha) {
    s <- sample(die, delta.n, replace = T, prob = biased.probs) # Throwing the die delta.n times
    s.factor <- factor(s, levels = 1:sides)                     # Factoring in possibly missing values
    observations.freq <- observations.freq + table(s.factor)    # Adding new observations to previous ones
    
    test <- chisq.test(observations.freq, p = base.probs)       # Goodness-of-fit test
    # N.B. The test does not account for the reason why the null hypothesis may
    # be rejected. In some cases, some other value than 1 may be observed so
    # often that the null hypothesis is rejected.
    a <- test$p.value
    # Continue while the p-value exceeds the significance level
  }
  final.n[b] <- sum(observations.freq) # Saving how many observations were required in the end
  
  # Bar plots for some values of bias (use "previous/next plot" buttons in RStudio):
  if(b %% round(length(bias)/5) == 1){ # %% is the modulus operator (remainder in division)
    barplot(observations.freq, main = paste("base = ", round(base.probs[1], digits=3),
                                           ", bias[", b,"] = +", round(bias[b], digits=3), sep = ""))
  }
}

plot(bias, final.n) # Basic plot

# Example on how to use the ggplot2 plotting library (should be pre-installed)
# for prettier plots. Full documentation: http://docs.ggplot2.org
library(ggplot2)
ggplot(mapping = aes(x = bias, y = final.n))+
  geom_line(col = "gray90")+                  # To be able to see the order of points.
  geom_point()+
  xlab("Bias added to the first side and substracted evenly from other sides")+
  ylab(paste("Number of observations required to detect the bias (multiple of ", delta.n,")", sep = ""))

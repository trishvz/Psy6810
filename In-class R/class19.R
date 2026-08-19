data <- read.csv('GSS_1980.csv')
library(effectsize)
library(pwrss)

######## Post-hoc power is a function of the p-value
sim_out.1 <- replicate(n = 2000, expr = {
  x1 <- rnorm(10, mean = 10, sd = 1)
  x2 <- rnorm(10, mean = 10.1, sd = 1)
  ttest.1 <- t.test(x1, x2, var.equal = TRUE)
  pwr.1 <- stats::power.t.test(delta = diff(ttest.1$estimate), 
                               sd = sqrt((var(x1) + var(x2))/2), 
                               sig.level = 0.05,
                               n = 10)
  c(pvalue = ttest.1$p.value, obs_power = pwr.1$power)
})

sim_out.2 <- replicate(n = 2000, expr = {
  x1 <- rnorm(10, mean = 10, sd = 1)
  x2 <- rnorm(10, mean = 10.1, sd = 1)
  ttest.2 <- t.test(x1, x2, var.equal = TRUE)
  pwr.2 <- stats::power.t.test(delta = diff(ttest.2$estimate), 
                               sd = sqrt((var(x1) + var(x2))/2), 
                               sig.level = 0.01,
                               n = 10)
  c(pvalue = ttest.2$p.value, obs_power = pwr.2$power)
})

# png(filename='post-hoc-power.png',width=800,height=800)
plot(t(sim_out.1),type='p',col='orange',
     xlim = c(0,1), ylim = c(0,1), 
     xlab = "p-value", ylab = "observed power",
     main = "Two-sample t test simulation")
abline(h=.5,v=.05,col=c('black','orange'))
points(t(sim_out.2),pch=2,col='blue')
abline(v=.01,col='blue')
# dev.off()

##########################
# Bootstrapped confidence interval

age <- data$AGE
N <- length(age) # sample size

bignum <- 1000
medians <- vector()
for (i in 1:bignum)
  medians[i] <- median(sample(age,N,replace=TRUE))

interval.95 <- quantile(medians,c(.025,.975))
interval.95

###################################
# Resampling

happy <- data$HAPPY < 3
money <- data$REALRINC

# Compute the mean difference
obs.diff <- mean(money[happy]) - mean(money[!happy])

bignum <- 1000
N.happy <- sum(happy)
N.total <- length(happy)

# Resample mean differences
diff <- vector()
for (i in 1:bignum) {
  choose <- sample(N.total,N.happy,replace=FALSE)
  diff[i] <- mean(money[choose]) - mean(money[-choose])
}

# Plot the histogram of differences
hist(diff,xlab="Mean Differences",freq=FALSE,main="")
abline(v=obs.diff)
text(obs.diff,0.000175,round(obs.diff,2),xpd=NA)

# Percentile rank of obs.diff
cat("The percentile rank of ",round(obs.diff,2)," is ",mean(diff < obs.diff),
    ", which corresponds to a 'p-value' of ",
    2*mean(diff >= obs.diff),".\n")

##########################
# Permutation test
library(MASS)
data(anorexia)

# Compute the difference scores
obs.diff <- mean(anorexia$Postwt - anorexia$Prewt)
N <- length(anorexia$Prewt)

bignum <- 1000
mean.diffs <- vector()
for (i in 1:bignum) {
  # Random vector of -1,+1
  signs <- 2*rbinom(N,1,.5) - 1
  mean.diffs[i] <- 
    mean(signs*(anorexia$Postwt - anorexia$Prewt))
}

# Plot the histogram of mean differences
hist(mean.diffs,xlab="Mean Differences",freq=FALSE,main="")
abline(v=obs.diff)
text(obs.diff,0.48,round(obs.diff,2),xpd=NA)

# Percentile rank of obs.diff
cat("The percentile rank of ",round(obs.diff,2)," is ",mean(mean.diffs < obs.diff),
    ", which corresponds to a 'p-value' of ",
    mean(mean.diffs >= obs.diff),".\n")


###############################
# Wilcoxon Rank Sum test
#
# The sample sizes for this test are usually on the small side.
# Look at only children who agree and disagree that young people should be 
# taught to respect the opinions of their elders
only <- data$SIBS==0

# Subset the data 
groups <- data$YOUNGEN[only] # 1 is "agree"
money <- data$REALRINC[only]

wilcox.test(money~groups,alternative="greater",correct=FALSE,exact=FALSE)
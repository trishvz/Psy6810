data <- read.csv('GSS_1980.csv')

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
    
# Percentile rank of obs.diff
cat("The percentile rank of ",round(obs.diff,2)," is ",
    mean(diff < obs.diff),
    ", which corresponds to a 'p-value' of ",
    mean(diff >= abs(obs.diff)) + mean(diff <= -1*abs(obs.diff)),
    ".\n")

t.test(money~happy)

##########################
# Permutation test
library("PairedData")
data(anorexia)

# Compute the difference scores
obs.diff <- mean(anorexia$Postwt - anorexia$Prewt)
N <- length(anorexia$Prewt)

bignum <- 10000
mean.diffs <- vector()
for (i in 1:bignum) {
  # Random vector of -1,+1
  signs <- 2*rbinom(N,1,.5) - 1 # Changes 0,1 to -1,1
  # If signs = -1 swap the pre and post, so the sign of the difference reverses,
  # but if signs = 1 leave it alone.
  mean.diffs[i] <- 
    mean(signs*(anorexia$Postwt - anorexia$Prewt))
}

# Plot the histogram of mean differences under the null
hist(mean.diffs,xlab="Mean Differences",breaks=50,freq=FALSE,main="",ylim=c(0,.45))
abline(v=obs.diff)
text(obs.diff,0.48,round(obs.diff,2),xpd=NA)

# Assume all measurements were drawn from a common normal distribution
# The distribution of the mean difference will then be normal with
mu <- 0 # No difference between the means
sigma2.hat <- var(c(anorexia$Prewt-anorexia$Postwt))/N

ds <- seq(-3,3,length=200)
lines(ds,dnorm(ds,mu,sqrt(sigma2.hat)))
# Percentile rank of obs.diff
cat("The percentile rank of ",round(obs.diff,2)," is ",mean(mean.diffs < obs.diff),
    ", which corresponds to a 'p-value' of ",
    mean(mean.diffs >= obs.diff),".\n")


##########################
# How the permutation test works, a simpler example
# Just 4 patients
X <- c(80,90,75,85)
Y <- c(85,88,80,87)
N <- 4

# Observed mean difference
obs.diff <- mean(Y)-mean(X)

# Each pair has a 50/50 chance of being swapped (permuted)
mean.diffs <- vector()
for (i in 1:bignum) {
  signs <- 2*rbinom(N,1,.5) - 1  # Change 0,1 to -1,1
  # If signs =1 don't swap, if signs=-1 swap
  mean.diffs[i] <- mean(signs*(Y-X))
}

hist(mean.diffs,xlab="Mean Differences",freq=FALSE,main="",
     breaks = seq(-3.75,3.75,by=.5))
abline(v=obs.diff)
text(obs.diff,0.55,round(obs.diff,2),xpd=NA)

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
N <- length(money)

# Figure out which group has the smaller N
smaller <- sum(groups==1) # Assume it's the agree group
small.group <- 1
# Check and swap to group 2 if it's smaller
if (smaller > N/2) {
  smaller <- N - smaller
  small.group <- 2
}

# Order the ranks for all only children
ranks <- rank(money,ties.method="average")

# Compute the summed ranks and its p-value
W.s <- sum(ranks[groups==small.group]) - smaller*(smaller+1)/2
W.s
1 - pwilcox(W.s,smaller,N-smaller)

# Compare with Wilcoxon function 
wilcox.test(money~groups,alternative="greater",correct=FALSE,exact=FALSE)

# Compare with resampling
bignum<- 10000
W <- vector()
for (i in 1:bignum) W[i] <- sum(sample(N,smaller,replace=FALSE))

# R shifts the sum in the smaller group by the smallest possible value of W.s
shift <- smaller*(smaller+1)/2
lower <- 0 # smallest shifted W.s
upper <- sum((N - smaller + 1):N) - shift # largest shifted W.s

W <- W - shift
# Plot the histogram of resampled values
hist(W,freq=FALSE,main="",breaks=50,
     xlab="Values of Ws",ylab="Probability",xlim=c(lower,upper))
abline(v=W.s)
text(W.s,.013,W.s,xpd=NA)

# Compute the p-value, the proportion of resampled values greater than the
# observed W.s
mean(W>=W.s)

###############################
# Wilcoxon Signed Rank Test
# Use the schizophrenia volume numbers
twin.1 <- c(1.94,1.44,1.56,1.58,2.06,1.66,1.75,
            1.77,1.78,1.92,1.25,1.93,2.04,1.62,
            2.08)
twin.s <- c(1.27,1.63,1.47,1.39,1.93,1.26,1.71,
            1.67,1.28,1.85,1.02,1.34,2.02,1.59,
            1.97)

####### By hand
# Determine the sign of each difference
signs <- sign(twin.1-twin.s)
# and the ranks of the difference magnitudes
ranks <- rank(abs(twin.1 - twin.s))

# Compute the sums of the positive and negative
# ranks and determine the minimum
T.pos <- sum(ranks[signs>0])
T.neg <- sum(ranks[signs<0])
T <- min(abs(c(T.pos,T.neg)))

# p-value (lower tail)
psignrank(T,length(twin.1))  

###### Using wilcox.test
wilcox.test(x=twin.s,y=twin.1,alternative="less",
            paired=TRUE,exact=TRUE,correct=FALSE)

########################
# Chi-squared goodness-of-fit test

# OSU vs. State of Ohio demographics (2014)
osu <- c(5449,6517,3895,3038,48,31)
ohio <- c(.134,.028,.048,.027,.003,.001)

# By hand
obs <- osu
# Compute expected frequencies
expect <- (ohio/sum(ohio))*sum(osu) # Note normalization of probabilities
rbind(obs,expect)

# Compute the chi-squared statistic and its p-value
X2 <- sum((obs-expect)^2/expect)
X2
1-pchisq(X2,length(expect)-1)  

# Using R
chisq.test(osu,p=ohio/sum(ohio))

###########################
# Chi-squared test of independence/association

hiv <- cbind("positive" = c(4,15),"negative"=c(73,33))
rownames(hiv) <- c("NoIV","IV")


# By hand
N <- sum(hiv) # total
status <- colSums(hiv) # HIV status marginal frequencies
drug <- rowSums(hiv) # IV drug use marginal frequencies

# Compute the expected joint frequencies
exp.hiv <- diag(status*drug)/N 
exp.hiv[1,2] <- drug[1]*status[2]/N
exp.hiv[2,1] <- drug[2]*status[1]/N

# Compute the chi-squared statistic and its p-value
X2 <- sum((hiv-exp.hiv)^2/exp.hiv)
X2

1-pchisq(X2,1)

# Using the chisq.test command
chisq.test(hiv,correct=FALSE)

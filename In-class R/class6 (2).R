# Sampling with vs without replacemenet

sample(5,5,replace=TRUE)
sample(5,5,replace=FALSE)


# Keno: Gambling!
balls <- 1:80
ohio <- sample(balls,20,replace=FALSE)

# Don't peek!  Pick your number of spots and your numbers
spots <- 5
my.picks <- sample(80,spots,replace=FALSE)

# Number of possible spot combos you could have picked
N <- gamma(81)/gamma(6)/gamma(76)

# How many spots did you match?
N.matches <- sum(my.picks %in% ohio)

# What's the probability of this number of matches?
# N.matches have to come from the 20, and (spots - N.matches) have to come from the remaining 60
hits <- gamma(21)/gamma(N.matches+1)/gamma(20 - N.matches+1) # Number of ways to pick the matches
misses <- gamma(61)/gamma(spots - N.matches+1)/gamma(60 - spots + N.matches+1) # Number of ways to pick the mismatches

# Number of ways we could have gotten N.matches
hits*misses

# Probility of N.matches
hits*misses/N

# Compare with the Hypergeometric distribution
dhyper(N.matches,20,60,spots)


# Using R's distribution functions

# Choose a mean and standard deviation
# IQ scores have mean 100 and sd 10
mu <- 100
sigma <- 10

# Select IQ scores for plotting
iq <- seq(mu-4*sigma,mu+4*sigma,length=200)

# Plot the density and distribution functions
par(mfrow=c(1,2))
plot(iq,dnorm(iq,mu,sigma),ylab="Density",xlab="IQ Score",type='l')
plot(iq,pnorm(iq,mu,sigma),ylab="Probability",xlab="IQ Score",type='l')
par(mfrow=c(1,1)) #reset to single plots

# Quantiles
p <- seq(0,1,by=.1)
cbind(100*p,qnorm(p,mu,sigma))

# Probabilities
# 1. Pr(IQ < 110)
pnorm(110,mu,sigma)
# 2. Pr(IQ > 95)
1 - pnorm(95,mu,sigma)
# 3. Pr(85 < IQ < 105)
pnorm(105,mu,sigma) - pnorm(85,mu,sigma)

# Random sampling
norm.sample <- rnorm(1000,mu,sigma)
hist(norm.sample,freq=FALSE,ylab="Relative Frequency",
     xlab="IQ Score",main="",ylim=c(0,.04))
lines(iq,dnorm(iq,mu,sigma),col='orange')

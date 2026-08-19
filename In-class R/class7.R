
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

# Random sampling from a normal distribution
norm.sample <- rnorm(1000,mu,sigma)
hist(norm.sample,freq=FALSE,ylab="Relative Frequency",
     xlab="IQ Score",main="",ylim=c(0,.04))
lines(iq,dnorm(iq,mu,sigma),col='orange')

# QQ Plot

data <- read.csv("GSS_1980.csv",header=TRUE)
age <- data$AGE # Age is positive and positively skewed

# Pick good values for a and k (gamma parameters)
a <- mean(age)/var(age)
k <- mean(age)^2/var(age)

obs <- quantile(age,seq(.025,.975,by=.05)) # Data quantiles
theory <- qgamma(seq(.025,.975,by=.05),k,a) # Gamma quantiles

# Contrast data histogram with gamma histogram
par(mfrow=c(1,2))
hist(age,freq=FALSE,breaks=seq(15.5,90.5,by=5),
     xlab="age",ylab="Density",main="")
lines(0:80,dgamma(0:80,k,a))

# Construct QQ plot
plot(theory,obs,type="p",ylab="Age Quantiles",xlab="Theoretical Gamma Quantiles")
abline(0,1)

# Sampling Distributions of the Mean and Variance
# Draw samples from an Exp(1) population
bignum <- 10000
N <- 10
means <- vector()
vars <- vector()

for (i in 1:bignum) {
  X <- rexp(N)
  means[i] <- mean(X)
  vars[i] <- var(X)
}

par(mfrow=c(1,3))

plot(seq(0,5,length=200),dexp(seq(0,5,length=200)),
     type ='l',xlab = "Values of X",ylab="Density",main="Population")

hist(means,breaks=seq(-.5,round(max(means))+.5,length=15),freq=FALSE,
     ylab="Relative Frequency",xlab="Values of mean(X)",main="Means")

hist(vars,breaks=seq(-.5,round(max(vars))+.5,length=15),freq=FALSE,
     ylab="Relative Frequency",xlab="Values of var(X)",
     main="Variances")


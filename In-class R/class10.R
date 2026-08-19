# Confidence intervals
# Draw 10000 samples of size 30 from an exponential distribution 
# with mean 5 and compute the sample means
bignum <- 10000 # Number of samples
N <- 30         # Sample size
lambda <- 5     # Population mean
conf <- .95     # Confidence level

# Sample from an Exp(lambda) distribution
x <- matrix(rexp(30*10000,1/lambda),ncol=30)

# Compute the means and standard errors for each sample
means <- rowMeans(x)
ses <- apply(x,1,sd)/sqrt(N) # Sample sd divided by sqrt(N)

# Plot the sampling distribution of the means
hist(means,freq=FALSE,breaks=25,main="",xlab="Sample Means")

# Variance known: sigma^2 = 25
z.limits <- qnorm(c((1-conf)/2,1-(1-conf)/2))
z.lower <- means + z.limits[1]*lambda/sqrt(N)
z.upper <- means + z.limits[2]*lambda/sqrt(N)

# Variance unknown: use s^2
t.limits <- qt(c((1-conf)/2,1-(1-conf)/2),N-1)
t.lower <- means + t.limits[1]*ses
t.upper <- means + t.limits[2]*ses

# Compute the probabilities that the z and t intervals 
# contain lambda
conf.z <- mean(lambda >= z.lower & lambda <= z.upper)
conf.t <- mean(lambda >= t.lower & lambda <= t.upper)

rbind(c("Confidence","Sigma known","Sigma unknown"),
      c(0.95,round(conf.z,2),round(conf.t,2)))

# The t-distribution requires that X be normally distributed,
# so redo
# Sample from an N(lambda,1) distribution
x <- matrix(rnorm(30*10000,lambda,1),ncol=30)

# Compute the means and standard errors for each sample
means <- rowMeans(x)
ses <- apply(x,1,sd)/sqrt(N) # Sample sd divided by sqrt(N)

# Variance known: sigma^2 = 1
z.limits <- qnorm(c((1-conf)/2,1-(1-conf)/2))
z.lower <- means + z.limits[1]*1/sqrt(N)
z.upper <- means + z.limits[2]*1/sqrt(N)

# Variance unknown: use s^2
t.limits <- qt(c((1-conf)/2,1-(1-conf)/2),N-1)
t.lower <- means + t.limits[1]*ses
t.upper <- means + t.limits[2]*ses

# Compute the probabilities that the z and t intervals 
# contain lambda
conf.z <- mean(lambda >= z.lower & lambda <= z.upper)
conf.t <- mean(lambda >= t.lower & lambda <= t.upper)

rbind(c("Confidence","Sigma known","Sigma unknown"),
      c(0.95,round(conf.z,2),round(conf.t,2)))

paste("Values of T = ",expression((bar(X)-mu)/(s/sqrt(N))))

# Plot a t-distribution with N-1 df
t <- seq(-4,4,length=200)
plot(t,dt(t,N-1),xlab="",ylab="Density",
     type='l',ylim=c(0,.4),frame=FALSE)
abline(v=qt(c(.025,.975),N-1))
text(0,-.125,xpd=NA,
     expression(T == frac(bar(X)-mu, s/sqrt(N))))
text(x=qt(c(.025,.975),N-1),y=.45,xpd=NA,
     c(round(qt(.025,N-1),2),round(qt(.975,N-1),2)))
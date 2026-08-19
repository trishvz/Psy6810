# More CLT
# Construct again the sampling distribution of the mean, samples drawn from a 
# Binomial(5,.2) distribution
bignum <- 100000 # infinity

# Binomial parameters and sample size
N <- 5
p <- .2
sample.size <- 50

# Use replicate() to obtain the means
x <- replicate(bignum,mean(rbinom(sample.size,N,p)))

# Plotting commands
par(mfrow=c(1,2))
# Binomial population
plot(seq(0,5),dbinom(0:5,N,p),type='p',
     xlab="Values of X",ylab="Probability")
segments(x0=0:5,y0=0,x1=0:5,y1=dbinom(0:5,N,p))
abline(h=0)

# Sampling distribution of mean
hist(x,probability=TRUE,breaks=seq(.47,1.63,by=.02),main="",
     xlab=expression("Values of "*bar(X)))
mean.values <- seq(0,N,length=200)
lines(mean.values,
      dnorm(mean.values,N*p,sqrt(N*p*(1-p)/sample.size)))
box()

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

# Confidence interval around the sample variance

bignum <- 10000
N <- 10
mu <- 100
sigma <- 15 # standard deviation

# Another way to simulate a sampling distribution
vars <- replicate(bignum,var(rnorm(N,mu,sigma)))

# "Critical" chi-squared values
chi.l <- qchisq(.025,N-1)
chi.u <- qchisq(.975,N-1)

# Lower and upper confidence limits
lower <- (N-1)*vars/chi.u
upper <- (N-1)*vars/chi.l

# Plot some of the confidence intervals
var.values <- seq(0,ceiling(max(vars)),length=200)

par(oma=c(3,3,3,3)) # all sides have 3 lines of space
par(mar=c(5,4,4,2) + 0.1)

plot(0,axes=FALSE,ann=FALSE,type="n",xlim=c(0,max(vars)),ylim=c(0,.0143))
lines(var.values,.01+dgamma(var.values,shape=(N-1)/2,scale=2*sigma^2/(N-1)))
abline(v=c(chi.l,chi.u)*sigma^2/(N-1),lty=2)
abline(v=sigma^2,lty=3)
abline(h=0.01)
text(x=c(chi.l,N-1,chi.u)*sigma^2/(N-1),y=.01477,xpd=NA,pos=3,
     c(expression(frac(sigma^2 * chi[.025]^2*"(N-1)",N-1)),
       expression(sigma^2),expression(frac(sigma^2 * chi[.975]^2*"(N-1)",N-1))))

text(sigma^2,-.001,expression(paste("Values of ",s^2,sep='')),xpd=NA)

# Add some of the observed variances to the plot
x <- vars[c(1:15,137)] 
y<- seq(.001,.009,length=16)
points(x,y,pch="|",col="purple4")
arrows(x0=lower[c(1:15,137)],y0=y,x1=upper[c(1:15,137)],y1=y,
       length=.1,angle=90,code=3,col="darkorange1")

# Compute the coverage proportion of the intervals
mean(sigma^2 >= lower & sigma^2 <= upper)




### Confidence intervals
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
# Sample from an N(lambda,lambda) distribution
x <- matrix(rnorm(30*10000,lambda,lambda),ncol=30)

# Compute the means and standard errors for each sample
means <- rowMeans(x)
ses <- apply(x,1,sd)/sqrt(N) # Sample sd divided by sqrt(N)

# Variance unknown: use s^2
t.limits <- qt(c((1-conf)/2,1-(1-conf)/2),N-1)
t.lower <- means + t.limits[1]*ses
t.upper <- means + t.limits[2]*ses

# Compute the probabilities that the new t intervals 
# contain lambda
conf.t <- mean(lambda >= t.lower & lambda <= t.upper)
rbind(c("Confidence","Sigma known","Sigma unknown"),
      c(0.95,round(conf.z,2),round(conf.t,2)))

### Confidence interval around the sample variance
# (Generate the figure in the slides)
# Set simulation parameters
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

### Confidence intervals for the GSS Age data
install.packages("DescTools")
library("DescTools")

data <- read.csv("GSS_1980.csv",header=TRUE)
age <- data$AGE

N <- length(age)
mean.age <- mean(age)
sd.age <- sd(age)

mean.limits <- mean.age + qt(c(.025,.975),N-1)*sd.age/N
var.limits <- sd.age^2*(N-1)/qchisq(c(.975,.025),N-1)

cat("Mean age = ",
    mean.age,", 95% CI [",
    mean.limits[1],",",mean.limits[2],"]\n",
  "Age variance= ",
    sd.age^2,", 95% CI [",
    var.limits[1],",",var.limits[2],"]")

# See also:
MeanCI(age)
VarCI(age)

### Tests of proportions

# Birth order data
X <- 10 # number of "successes"
N <- 15 # number of families

# The birth order effect implies that p > .5, a "greater" 
# alternative hypothesis
# Without correction for continuity:
prop.test(X,N,alternative="greater",correct=FALSE)

# With the Yates correction:
prop.test(X,N,alternative="greater",correct=TRUE)

# Binomial exact test:
binom.test(X,N,alternative="greater")
# Add up all the binomial probabilities for values of X
# greater than or equal to 10:
sum(dbinom(10:15,15,.5)) # or
1 - pbinom(9,15,.5)
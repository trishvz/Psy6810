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
  X <- rexp(N) # Draw a random sample of size N
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


# Figures for CLT.pdf
# Binomial Probability Function

N <- 10
p <- .3
x <- 0:10

plot(x,dbinom(x,N,p),type="p",xlab="Values of X",ylab="Probability")
segments(x0=x,y0=0,x1=x,y1=dbinom(x,N,p))
abline(h=0)


# Poisson Probability Function

lambda <- 2
x <- 0:10

plot(x,dpois(x,lambda),type="p",xlab="Values of X",ylab="Probability")
segments(x0=x,y0=0,x1=x,y1=dpois(x,lambda))
abline(h=0)

# Normal Density function

mu <- 100
sigma <- 15
x <- seq(mu - 4*sigma,mu+4*sigma,length=200)

plot(x,dnorm(x,mu,sigma),type="l",xlab="Values of X",ylab="Density")
abline(h=0)

z <- (x - mu)/sigma
plot(z,dnorm(z,0,1),type="l",xlab="Values of Z",ylab="Density")
abline(h=0)

# Student's t

t <- seq(-5,5,length=200)
nu.1 <- 8
nu.2 <- 5
nu.3 <- 30

plot(t,dnorm(t),type="l",xlab="Values of T",ylab="Density",col='black')
abline(h=0)
lines(t,dt(t,nu.2),col='darkolivegreen')
lines(t,dt(t,nu.3),col='orange')
lines(t,dt(t,nu.1),col='blue')
legend("topright",legend=c("8","15","30",expression(infinity)),
       lty=1,col=c("blue","darkolivegreen","orange","black"))

# chi-squared

nu.1 <- 4
nu.2 <- 10
x <- seq(0,nu.2 + 4*sqrt(2*nu.2),length=200)

plot(x,dchisq(x,nu.1),type="l",xlab=paste("Values of ",expression(X^2),sep=''),
     ylab="Density",col='black')
lines(x,dchisq(x,nu.2),col="blue")
abline(h=0)
legend("topright",legend=c("4","10"),
       lty=1,col=c("black","blue"))


# F

num.1 <- 4
num.2 <- 4
den.1 <- 10
den.2 <- 20

f <- seq(0,10,length=200)

plot(f,df(f,num.2,den.2),type="l",xlab="Values of F",
     ylab="Density",col='black')
lines(x,df(f,num.1,den.1),col="blue")
abline(h=0)
legend("topright",legend=c("4,10","4,20"),
       lty=1,col=c("blue","black"))











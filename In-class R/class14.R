# Simulate the t-distribution and show that "little" s^2 is an unbiased
# estimator of the population variance
bignum <- 10000 # number of simulated experiments
N <- 15 # sample size
pop.var <- 93 # population variance
pop.mean <- 47 # population mean

s2 <- S2 <- t.stat <- x.bar <- vector()

# Sample from a normal(pop.mean,pop.variance) distribution bignum times
# Compute the sample mean and variances and t-statistic
for (i in 1:bignum) {
  X <- rnorm(N,pop.mean,sqrt(pop.var))
  x.bar[i] <- mean(X)
  s2[i] <- var(X)
  S2[i] <- sum((X-x.bar[i])^2)/N # "Big S" squared, "population" variance
  t.stat[i] <- (x.bar[i] - pop.mean)/sqrt(s2[i]/N)
}

# What is the mean of the means?
cat("The population mean is ",pop.mean,
    " and the mean of the means is ",mean(x.bar))

# What is the mean of the variances?
cat("The population variance is ",pop.var,
    " and the mean of the sample variances is ",mean(s2))

# What is the mean of the "population" variances
cat("The population variance is ",pop.var,
    " and the mean of the 'population' variances is ",mean(S2))

# What is the distribution of the t-statistics? Show that the degrees of 
# freedom of the t-statistic is N-1
hist(t.stat,breaks=50,main="",xlab="Values of T",freq=FALSE)
ts <- seq(-4,4,length=200) # For plotting
# You can play around with the df and show that other t-distributions
# don't fit over the histogram as well:
lines(ts,dt(ts,N-1),col="blue") 

#############################
# T-tests using the GSS data
data <- read.csv('GSS_1980.csv')

# One sample t-test for the age variable: H0: mu <= 30 vs. H1: mu > 30
t.test(data$AGE,mu=30,alternative="greater")

# Two-sample t-test of money buying happiness
# Collapse the HAPPY variable and rename income 
happy <- data$HAPPY < 3
money <- data$REALRINC

# Determine sample sizes
total.N <- length(happy)
N.happy <- sum(happy)
N.unhappy <- total.N - N.happy

# Compute sample means
means <- aggregate(money,by=list(happy),mean)

# Compute sample variances
vars <- aggregate(money,by=list(happy),var)

# Pool the variances
sp2 <- ((N.happy - 1)*vars$x[2] + (N.unhappy - 1)*vars$x[1])/(total.N-2)

# Compute standard error estimate
se <- sqrt(sp2*(1/N.happy + 1/N.unhappy))

# Compute t
t.money <- (means$x[2] - means$x[1])/se

# Compute p-value
p.money <- 1 - pt(t.money,total.N-2)

# Compare results with:
t.test(money~happy,alternative="less",var.equal=TRUE)

# But what if variances aren't equal?
# Satterthwaite Correction
# Redefine some variables to make it easier
group.1 <- money[happy==TRUE]
group.2 <- money[happy==FALSE]

# New degrees of freedom
df.prime <- ((var(group.1)/N.1 + var(group.2)/N.2)^2)/
((var(group.1)/N.1)^2/(N.1-1) + (var(group.2)/N.2)^2/(N.2-1))  

# New estimate of standard error
se.prime <- sqrt(var(group.1)/N.1 + var(group.2)/N.2)

# t-statistic
t.prime <- (mean(group.1) - mean(group.2))/se.prime

# p-value
p.prime <- 1 - pt(t.prime,df)

# Compare with t.test
t.test(money~happy,alternative="less")

##############################
# Role of p-value (picture in notes)

# Pick two significance levels and figure out what the sample
# means are for those if mu0 = 38 (the null)
# Note the sample sizes are so large the t = z.  Use the 
# standard error from pooling the variance above.
z.1 <- 38+se*qnorm(.98) 
z.2 <- 38+se*qnorm(.998)

# Plot the null and alternative distributions where mu1 = 39.
ts <- seq(37,41,length=200)
plot(ts,dnorm(ts,38,se),type='l',ylab="",
     xlab=expression(paste("Values of ",bar(X))),lty=3)
lines(ts,dnorm(ts,39,se))
abline(v=c(z.1,z.2),lty=2)

# Highlight the p-values (0.02 and 0.002)
polygon(c(z.1,ts[ts>=z.1],z.1),
        c(0,dnorm(ts[ts>=z.1],38,se),0),angle=135,density=30)
polygon(c(z.2,ts[ts>=z.2],z.2),
        c(0,dnorm(ts[ts>=z.2],38,se),0),col="red")
abline(h=0,col="gray80")

#######################################
# Sampling distribution of the mean difference
# Set population and simulation parameters
bignum <- 10000

# Sample sizes
N.1 <- 15 
N.2 <- 20

# Population means
mu.1 <- 40
mu.2 <- 35

# Common population variance
sigma <- 9

# Simulate bignum samples
mean.d <- s.1 <- s.2 <- vector()
for (i in 1:bignum) {
  # Normal distribution for data
  X.1 <- rnorm(N.1,mu.1,sigma)
  X.2 <- rnorm(N.2,mu.2,sigma)
  
  # Compute mean difference and sds
  mean.d[i] <- mean(X.1) - mean(X.2)
  s.1[i] <- sd(X.1)
  s.2[i] <- sd(X.2)
}

ds <- seq(-5,15,length=200) # for plotting

# Sampling distribution of mean difference
hist(mean.d,freq=FALSE,breaks=50,main="",
     xlab=expression(
       paste("Values of ",bar(X)-bar(X))))
lines(ds,dnorm(ds,mu.1-mu.2,sigma*sqrt(1/N.1 + 1/N.2)))
abline(v=mu.1-mu.2,col="blue")

cat("The mean of the mean differences is ",
    mean(mean.d),
    paste(" and ",expression(mu_1-mu_2)," is ",mu.1-mu.2))
cat("The variance of the mean differences is ",
    var(mean.d),
    paste(" and ",expression(sigma^2 (1/N_1 + 1/N_2)),
          " is ",sigma^2 * (1/N.1 + 1/N.2)))


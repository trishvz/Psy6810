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
df.prime <- ((var(group.1)/N.happy + var(group.2)/N.unhappy)^2)/
((var(group.1)/N.happy)^2/(N.happy-1) + (var(group.2)/N.unhappy)^2/(N.unhappy-1))  

# New estimate of standard error
se.prime <- sqrt(var(group.1)/N.happy + var(group.2)/N.unhappy)

# t-statistic
t.prime <- (mean(group.1) - mean(group.2))/se.prime

# p-value
p.prime <- 1 - pt(t.prime,df.prime)

# Compare with t.test
t.test(money~happy,alternative="less")

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

########################################
# Dependent samples; fake Alzheimer's data

before <- c(36,46,76,58,49,35,46,51,60,32)
after <- c(42,46,75,65,55,33,49,53,54,40)
alzh <- data.frame(speed=c(before,after),
                   before=c(rep(1,times=length(before)),
                            rep(0,times=length(after))))

t.test(x=before,y=after,mu=0,data=alzh,paired=TRUE,alternative='less')


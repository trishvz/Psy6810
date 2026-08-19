# How would you test for a difference in variability between two groups?
# Bootstrap test for difference between ranges of two groups
# Load data
data <- read.csv('GSS_1980.csv')
attitude <- data$YOUNGEN
siblings <- data$SIBS

# H0: no difference between ranges of two groups
# H1: people who endorse the traditional attitude (1) have a larger range of siblings

N.1 <- length(siblings[attitude==1])
N.2 <- length(siblings[attitude==2])

######### Bootstrapping difference between ranges ###

# 95% confidence interval
diff.range <- replicate(10000,
{
  # Sample with replacement from each group
  X <- sample(siblings[attitude==1],N.1,replace=TRUE)
  Y <- sample(siblings[attitude==2],N.2,replace=TRUE)
  # Compute difference between ranges
  diff(range(X)) - diff(range(Y))
})
# 95% Confidence interval
quantile(diff.range,c(0.025,0.975)) # Includes 0 => no significant difference

# Independent samples permutation test
# Resample under H0: no difference between groups
# Observed difference between ranges
obs.diff <- diff(range(siblings[attitude==1])) -
  diff(range(siblings[attitude==2]))

diff.range <- replicate(10000,
{
  # Combine data
  combined <- sample(siblings, N.1 + N.2, replace=FALSE)
  # Split into two groups
  X <- combined[1:N.1]
  Y <- combined[(N.1+1):(N.1+N.2)]
  # Compute difference between ranges
  diff(range(X)) - diff(range(Y))
})

# Plot histogram of bootstrap differences under H0: no difference
hist(diff.range,freq=FALSE,
     breaks=seq(min(diff.range)-1,max(diff.range)+1,by=1))
abline(v=obs.diff,col="red",lwd=2)

# Compute p-value for upper tail test
p.value <- mean(diff.range >= obs.diff)
p.value

##########################
# F-test for equality of variances
data<-read.csv('GSS_1980.csv')
X <- data$REALRINC[data$YOUNGEN==1]
Y <- data$REALRINC[data$YOUNGEN==2]

# Compute variances and sample sizes
s2x <- var(X)
s2y <- var(Y)
Nx <- length(X)
Ny <- length(Y)

# By hand
F <- s2x/s2y
F
p <- 1-pf(F,Nx-1,Ny-1)
if (F < 1) p <- pf(F,Nx-1,Ny-1)
p # is this value less than alpha?

# Using var.test
var.test(X,Y)



################
# F-test is always 1-tailed: put the big variance on top vs. always s1 on top

p.1 <- p.2 <- vector() # p-values
bignum <- 10000
for (i in 1:bignum) {
   # Generate two samples
   X <- rnorm(15,0,10)
   Y <- rnorm(10,0,10)
   
   # F ratio, X on top
   F.1 <- var(X)/var(Y)
   if (F.1 > 1) { # compute p-values on upper tail, big variance is on top
     F.2 <- F.1
     p.1[i] <- p.2[i] <- 1-pf(F.1,14,9)
   } else { # compute p-values on lower tail, invert for big variance on top
     F.2 <- 1/F.1
     p.1[i] <- pf(F.1,14,9) # lower tail rejection region
     p.2[i] <- 1 - pf(F.2,9,14) # upper tail rejection region
   }
}
mean(p.1 < .025) # Standard two-tailed test (should be 0.05)
mean(p.2 < .05) # Converted to always upper tailed (will be twice what is expected)

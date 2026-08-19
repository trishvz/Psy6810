# Outliers
# Our shoe size data, with Robert at the end
shoes <- c(6,7,7.5,7.5,8,8.5,9,9.5,10,10,37)

# Compare the means and standard deviations with and without Robert:
# Means
mean.class <- mean(shoes[1:10])
mean.robert <- mean(shoes)

# SDs
sd.class <- sd(shoes[1:10])
sd.robert <- sd(shoes)

# Four sds above the mean with and without Robert
mean.class + 4*sd.class    # Robert doesn't look like an outlier
mean.robert + 4*sd.robert  # Robert absolutely looks like an outlier
# Having Robert in the sample greatly affects the mean and sd

# Compute Robert's z-score *relative to the class*
(shoes[11] - mean.class)/sd.class # Over 21 sds above the mean

# Using quartiles (box plot approach, pp.47-51 in Howell)
boxplot(shoes) # Robert will appear as the high dot above the box-and-whiskers

# Bernoulli's Theorem
# We're going to "roll" a virtual die
# Set some values for N, the number of times we'll roll the die
N <- c(12,18,24,6000)

# Initialize a variable as nothing
die <- NULL

# Roll the die N times for each value of N
for (i in 1:length(N)) {
  # Compute relative frequencies with hist(), which will let you 
  # define intervals that could contain 0
  # rbind will "bind" strings of values into a row
  # sample will select numbers from 1-6 (in this case) at random
  # hist will make a histogram, but in this case we only want it to count
  #      observations within each "bin" defined by the breakpoints
  die <- rbind(die,hist(sample(1:6,N[i],replace=TRUE),
                  breaks=seq(.5,6.5,by=1),plot=FALSE)$counts/N[i])
}
die # take a look at what you created

# Plot the relative frequency distributions in a single barplot
barplot(t(die), beside = TRUE, col = "gray",
        names.arg = c("N=12", "N=18", "N=24","N=6000"),
        ylab = "Relative Frequency", xlab = "Spots on Die")

# Add a horizontal line at the value 1/6
abline(h=.166667,col="black")

# Using R's normal distribution functions

# Choose a mean and standard deviation
# IQ scores have mean 100 and sd 10
mu <- 100
sigma <- 10

# Select IQ scores for plotting
# seq makes a sequence of values of specified length (or use by= to control
#   the size of the intervals)
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

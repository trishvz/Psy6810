# These chunks of code compute the sum of squared deviations and the sum of squared deviations around arbitrary centers.  The values of these sums are plotted against the values of the centers.  It demonstrates that the mean (sum(X)/N) is the center for which the sum of deviations is equal to zero and for which the sum of squared deviations is as small as possible.

# Read in the GSS data as usual
data <- read.csv('GSS_1980.csv',header=TRUE)
data$X <- NULL
names(data) <- c("income","sibs","age","happy","health","taught")

# Pull out the age variable
ages <- data$age

# Compute the mean and median age
mean.age <- mean(ages)
median.age <- median(ages)

# A function to compute the sum of deviations around a center 
sum.devs <- function(center,X) {
  # Compute the deviations for each center value
  devs <- vector()
  for (i in 1:length(center)) { # For every value
  devs[i] <- sum(X - center[i]) # compute the sum of deviations
  }
  return(devs)
}

# A function to compute the sum of squared deviations around a center 
sum.2.devs <- function(center,X) {
  # Compute the squared deviations for each center value
  devs <- vector()
  for (i in 1:length(center)) {         # For every value
    devs[i] <- sum((X - center[i])^2)   # compute the sum of squared deviations
  }
  return(devs)
}


# Define a range of values for the center (all observed ages plus values in between)
x.ages <- seq(min(ages),max(ages),by=.1) # the centers

# Plot the sums of deviations
plot(x.ages,sum.devs(x.ages,ages),type='l',xlab='Center Value',ylab='Sum of Deviations',
     main='Sum of Deviations around Center')
abline(h=0,v=mean.age,col="red")
text(mean.age,-40000,bquote(bar(X)==.(round(mean.age,2))),pos=4)

# Plot the sums of squared deviations
plot(x.ages,sum.2.devs(x.ages,ages),type='l',xlab='Center Value',ylab='Sum of Squares',
     main='Sum of Squares around Center')
abline(v=mean.age,col="red")
text(mean.age,500000,bquote(bar(X)==.(round(mean.age,2))),pos=4)

# Exercise: Using the functions above as a model, code a function that computes the sum of the absolute value of the deviations (abs(X - center[i])).  Plot the values of this function for all the observed ages in x.ages and overlay the median age (median.age).  Which center minimizes the sum of absolute deviations?
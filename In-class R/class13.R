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
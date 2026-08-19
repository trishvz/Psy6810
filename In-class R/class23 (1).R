########################
# Generate Fake Fear of Heights Data
set.seed(347065) # Make sure it's the same every time you run this code
X <- c(rnorm(80,40,10),rnorm(20,50,10))
group <- rep(1:5,each=20)
N.i <- 20
N.total <- 100
N.groups <- 5

# ANOVA by hand
# Compute grand and group means
grand.mean <- mean(X)
group.means <- aggregate(X,by=list(group),mean)$x

# Total sum of squares
SS.total <- sum((X - grand.mean)^2)
SS.total

# Between groups sum of squares
SS.between <- sum(N.i*(group.means - grand.mean)^2)
SS.between

# Within groups sum of squares
SS.within <- 0
for (i in 1:N.groups) SS.within <- SS.within + sum((X[group==i] - group.means[i])^2)

# Note that SS.within - SS.total - SS.between
SS.within
SS.total - SS.between

# Compute mean squares
MS.between <- SS.between/(N.groups - 1)
MS.within <- SS.within/(N.total - N.groups)

# Compute the F-ratio
F <- MS.between/MS.within

# Compute the p-value
1 - pf(F,N.groups-1,N.total-N.groups)

# ANOVA in R the right way
group.factor <- as.factor(group) # generate a categorical variable from group
results <- aov(X~group.factor)
summary(results)

# ANOVA in R the wrong way 
results <- aov(X~group) # group is treated as a numerical variable
summary(results)

########## Effect sizes and ncp by hand
eta.2 <- SS.between/SS.total

omega.2 <- (SS.between - (N.groups-1)*MS.within)/
           (SS.total + MS.within)

################### Effect sizes
library('effectsize')
library('pwrss')

eta_squared(results)
omega_squared(results)

#################### Power
# Post-hoc
power.f.ancova(eta.squared = 0.2011,factor.levels=5,
               k.covariates=0,n.total=100)

# A priori
power.f.ancova(eta.squared = 0.2011,factor.levels=5,
               k.covariates=0,power=0.80)



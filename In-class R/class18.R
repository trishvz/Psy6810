########################################
# Dependent samples; fake Alzheimer's data

before <- c(36,46,76,58,49,35,46,51,60,32)
after <- c(42,46,75,65,55,33,49,53,54,40)
alzh <- data.frame(speed=c(before,after),
                   before=c(rep(1,times=length(before)),
                            rep(0,times=length(after))))

###########################
# Cohen's d

library('effectsize')
data <- read.csv("GSS_1980.csv")

happy <- as.numeric(data$HAPPY<3)
money <- data$REALRINC

Ntotal <- length(money)
Nhappy <- sum(happy)
Nunhappy <- Ntotal - Nhappy

mean.happy <- mean(money[happy==1])
mean.unhappy <- mean(money[happy!=1])

sd.happy <- sd(money[happy==1])
sd.unhappy <- sd(money[happy!=1])

# Regular pooled variance
# sp2 <- ((Nhappy-1)*sd.happy^2+(Nunhappy-1)*sd.unhappy^2)/(Ntotal-2)
sp2 <- (sd_pooled(money~happy))^2

# Pooled variance without subtracting 2
sp2.star <- sp2*(Ntotal-1)/Ntotal

# Unpooled variance for Welch's t
# (Convention is to just average without weighting)
sd.tilde <- sqrt((sd.happy^2+sd.unhappy^2)/2)

# Cohen's d
d <- (mean.happy-mean.unhappy)/sqrt(sp2)

# Another Cohen's d (no subtraction of 2)
(mean.happy-mean.unhappy)/sqrt(sp2.star)

# Cohen's d when variance is NOT pooled (Welch's)
(mean.happy-mean.unhappy)/sd.tilde

library('effectsize')

# Note: average US salary in 1980: $12,513
t.test(money,mu=12513)
cohens_d(money,mu=12513)

t.test(money~happy,alternative="less")
cohens_d(money~happy,alternative="less",pooled_sd=FALSE)
##################

#######################
# Hedges g

df <- Ntotal-2

# Log correction (df is too big for gamma() function)
lJ <- lgamma(df/2)-log(sqrt(df/2)) - lgamma((df-1)/2)
g <- exp(lJ)*d

g
hedges_g(money~happy)

#########################################
# Glass' Delta
# Use sd.happy as the "more stable" estimate
# because Nhappy >> Nunhappy
(mean.happy-mean.unhappy)/sd.happy
glass_delta(money~happy,alternative='less')


########### Power
library(pwrss)


##########################
# Power Z-test

N <- 100
d <- .10

# Noncentrality parameter
ncp <- d*sqrt(N)

power.z.test(ncp=ncp,alpha=.05,alternative='one.sided')


#############################
# Noncentral t figure
N <- 10
d <- 0.8 # effect size

# Noncentrality parameter
ncp <- d*sqrt(N)

# Plot commands for noncentral T
t <- seq(-4,7,length=200) # for plotting
plot(t,dt(t,N-1),xlab="Values of T",ylab="Density",type='l')
lines(t,dt(t,N-1,ncp),lty=2)
legend('topright',c("Central t(N-1)","Noncentral t(N-1)"),lty=c(1,2))
abline(v=qt(.95,N-1))
text(4.3,.2,"d=0.8")
###########################

#############################
# Power for 2-sample T-test
# Katz (1990) example has to be done by hand because we don't
# have the raw data

# Enter in sample statistics
N.1 <- 17
N.2 <- 28
mean.1 <- 69.6
mean.2 <- 46.6
s.1 <- 10.6
s.2 <- 6.8

# Pool the variance
sp <- sqrt((16*10.6^2 + 27*6.8^2)/(17+28-2))
df <- N.1 + N.2 - 2

t <- (mean.1 - mean.2)/(sp*sqrt(1/N.1 + 1/N.2))
1 - pt(t,df) # p-value for upper-tailed test

# Cohen's d
d <- abs((mean.2 - mean.1)/sp)
# OR
# d <- abs((mean.2 - mean.1)/sqrt(mean(c(s.1^2,s.2^2))))

# Noncentrality parameter
ncp <- d * sqrt((N.1*N.2)/(N.1 + N.2))
ncp 

# Power computation
power.t.welch(d=d,var.ratio=s.2^2/s.1^2,n.ratio=N.2/N.1,
              n2=17,alpha=0.05,alternative='one.sided')

#####################################
# Sample size determination for z-test

alpha <- 0.01
desired.power <- 0.95
d <- 1.0 # effect size to be detected
N <- seq(10,25) 
ncp <- d * sqrt(N)

# Compute power for each N
power <- vector()
for (i in 1:length(N)) {
  ncp <- d*sqrt(N[i])
  power[i] <- power.z.test(ncp=ncp,alpha=alpha,alternative='one.sided',
                           verbose=FALSE,plot=FALSE)$power
}

# Plot power curve
first<-which(power>=desired.power)[1]
plot(N,power,xlab="Sample Size",ylab="Power",type='l')
points(N[(first-1):first],power[(first-1):first])
abline(h=desired.power,lty=2)
abline(v=c(N[first],N[first-1]),lty=3)
axis(1,at=16,tick=TRUE)

#########################################
# Sample size determination for two-sample t-test

alpha <- 0.05
desired.power <- 0.80
d <- 0.5

# pwrss
power <- power.t.welch(d=d,alpha=alpha,alternative='one.sided',
                       n.ratio=1,power=desired.power)



########################################
# Dependent samples; fake Alzheimer's data

before <- c(36,46,76,58,49,35,46,51,60,32)
after <- c(42,46,75,65,55,33,49,53,54,40)
alzh <- data.frame(speed=c(before,after),
                   before=c(rep(1,times=length(before)),
                            rep(0,times=length(after))))

t.test(before,mu=59,alternative="less")
t.test(x=before,y=after,mu=0,data=alzh,paired=TRUE,alternative='less')

# Compare variances
var(before)
var(after)
cor(before,after)

var(before) + var(after) - 2*cor(before,after)*sd(before)*sd(after)
var(before-after)

cohens_d(x=before,y=after,pooled_sd=FALSE)

###########################
# Cohen's d
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
sp2 <- ((Nhappy-1)*sd.happy^2+(Nunhappy-1)*sd.unhappy^2)/(Ntotal-2)

# Pooled variance without subtracting 2
sp2.star <- sp2*(Ntotal-1)/Ntotal

# Glass Delta variance
sd.tilde <- sqrt((sd.happy^2+sd.unhappy^2)/2)

# Cohen's d
d <- (mean.happy-mean.unhappy)/sqrt(sp2)

# Another Cohen's d
(mean.happy-mean.unhappy)/sqrt(sp2.star)

# Cohen's d when variance is NOT pooled
(mean.happy-mean.unhappy)/sd.tilde


library('effectsize')
cohens_d(money~happy,alternative='less',pooled_sd=TRUE)

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
(mean.happy-mean.unhappy)/sd.happy
glass_delta(money~happy,alternative='less')


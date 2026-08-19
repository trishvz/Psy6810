########################
# Generate Fake Fear of Heights Data
set.seed(347065) # Make sure it's the same every time you run this code
X <- c(rnorm(80,40,10),rnorm(20,50,10))
group <- as.factor(rep(1:5,each=20))
N.i <- 20
N.total <- 100
N.groups <- 5

# Fit the ANOVA model and display the ANOVA summary table
results <- aov(X ~ group)
summary(results)

# ANOVA as regression
summary.lm(results)

#################### Power using pwrss package
# Post-hoc
power.f.ancova(eta.squared = 0.2011,factor.levels=5,
               k.covariates=0,n.total=100)

# A priori
power.f.ancova(eta.squared = 0.20,factor.levels=5,
               k.covariates=0,power=0.80)

################## Post hoc comparisons

# Bonferroni Correction
# pairwise.t.test(X,group,p.adjust.method = "none")
pairwise.t.test(X,group,p.adjust.method = "bonferroni")

############ Studentized range density
# Only has a closed form for nu = 2 and 3
# These densities are estimated by the "derivative" of the cdf
q <- sqrt(6) * seq(0,sqrt(6),length=1000)
est.3.4 <- (ptukey(q[2:1000],3,4)-ptukey(q[1:999],3,4))/(q[2] - q[1])
est.6.4 <- (ptukey(q[2:1000],6,4)-ptukey(q[1:999],6,4))/(q[2] - q[1])
est.3.15 <- (ptukey(q[2:1000],3,15)-ptukey(q[1:999],3,15))/(q[2] - q[1])
est.6.15 <- (ptukey(q[2:1000],6,15)-ptukey(q[1:999],6,15))/(q[2] - q[1])

q.midpoints <- (q[1:999]+q[2:1000])/2
plot(q.midpoints,est.6.15,xlab="Values of q",type='l',ylab="",lty=2)
lines(q.midpoints,est.3.15)
lines(q.midpoints,est.3.4,col="orange4")
lines(q.midpoints,est.6.4,col="orange4",lty=2)
legend("topright",c("k=3,nu=4",
                    "k=6,nu=4",
                    "k=3,nu=15",
                    "k=6,nu=15"),
       lty=c(1,2,1,2),
       col=c("orange4","orange4","black","black"))

############ Differences of Means Matrix
means <- aggregate(X,by=list(group),mean)$x

mean.mx <- round(outer(means,rep(1,times=5)) - 
                 t(outer(means,rep(1,times=5))),2)

# Tukey's HSD
qtukey(.95,5,95) * sqrt(105.3/20) # for alpha=0.05

TukeyHSD(results,ordered=TRUE)

############ GSS data
data <- read.csv("GSS_1980.csv")
money <- data$REALRINC
happy <- as.factor(data$HAPPY < 3)

gss <- aov(money~happy)
summary(gss)
t.test(money~happy,var.equal=TRUE)

###### Jury data
juror <- rep(1:5,times=3)
group <- rep(c("Record","Clean","Control"),each=5)
rating <- c(10,7,5,10,8,
            5,1,3,7,4,
            4,6,9,3,3)
data <- data.frame("Juror"=juror,"Group"=group,"Rating"=rating)


data <- read.csv('GSS_1980.csv')

happy <- data$HAPPY < 3
money <- data$REALRINC


########################
# Chi-squared goodness-of-fit test

# OSU vs. State of Ohio demographics (2014)
osu <- c(5449,6517,3895,3038,48,31)
ohio <- c(.134,.028,.048,.027,.003,.001)

# By hand
obs <- osu
# Compute expected frequencies
expect <- (ohio/sum(ohio))*sum(osu) # Note normalization of probabilities
rbind(obs,expect)

# Compute the chi-squared statistic and its p-value
X2 <- sum((obs-expect)^2/expect)
X2
1-pchisq(X2,length(expect)-1)  

# Using R
chisq.test(osu,p=ohio/sum(ohio))

###########################
# Chi-squared test of independence/association

hiv <- cbind("positive" = c(4,15),"negative"=c(73,33))
rownames(hiv) <- c("NoIV","IV")


# By hand
N <- sum(hiv) # total
status <- colSums(hiv) # HIV status marginal frequencies
drug <- rowSums(hiv) # IV drug use marginal frequencies

# Compute the expected joint frequencies
exp.hiv <- diag(status*drug)/N 
exp.hiv[1,2] <- drug[1]*status[2]/N
exp.hiv[2,1] <- drug[2]*status[1]/N

# Compute the chi-squared statistic and its p-value
X2 <- sum((hiv-exp.hiv)^2/exp.hiv)
X2

1-pchisq(X2,1)

# Using the chisq.test command
chisq.test(hiv,correct=FALSE)

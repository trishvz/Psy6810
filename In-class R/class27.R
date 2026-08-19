## Three-way factorial ANOVA

data <- read.csv("GSS_1980.csv")
# Convert to factors
happy <- as.factor(data$HAPPY)
health <- as.factor(data$HEALTH)
attitude <- as.factor(data$YOUNGEN)
age <- data$AGE

# Fit the ANOVA model
gss.model <- aov(age~(happy + health + attitude)^3)
summary(gss.model)

# Fit the ANOVA model as regression
gss.glm <- glm(age~(happy + health + attitude)^3)
summary(gss.glm)

### Recognition Memory
recog <- c(
43 , 52 , 62 , 
47 , 65 , 75 ,
62 , 71 , 79 ,
55 , 67 , 71 ,
36 , 46 , 57 )
participant <- as.factor(rep(1:5,each=3))
times <- as.factor(rep(c(500,1000,1500),times=5))

# See: https://conjugateprior.org/2013/01/formulae-in-r-anova/
recog.model <- aov(recog~times+Error(participant/times))
summary(recog.model)



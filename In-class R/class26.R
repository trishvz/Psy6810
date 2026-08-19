###### Jury data
juror <- rep(1:5,times=3)
group <- as.factor(rep(c("Record","Clean","Control"),each=5))
rating <- c(10,7,5,10,8,
            5,1,3,7,4,
            4,6,9,3,3)
data <- data.frame("Juror"=juror,"Group"=group,"Rating"=rating)

# Note that factors are ordered alphabetically for character data
levels(data$Group)

# Level 1 = Clean, Level 2 = Control, Level 3 = Record

# Planned contrasts (orthogonal)
c1 <- c(-.5,-.5,1) # Clean+Control = Record
c2 <- c(-1,1,0) # Clean = Control
mat <- cbind(c1,c2)

# Tell R what the contrasts of interest are
contrasts(data$Group) <- mat

# Compute the values of Psi
means <- aggregate(data$Rating,by=list(data$Group),mean)$x
Psi <- means%*%mat # Matrix multiplication
Psi

jury.model <- aov(Rating~Group,data=data)
summary.aov(jury.model,
            split=list(Group=list("Record=Clean+Control"=1,
                                  "Clean=Control"=2)))

# Simulated by-subject Eysenck data (from Howell, p. 415)
task <- rep(rep(c("Counting","Rhyming","Adjective","Imagery","Intentional"),each=10),times=2)
age <- rep(c("Older","Younger"),each=50)

recall <- 
 c(9 ,   8 ,   6 ,   8 ,   10,   4 ,   6 ,   5 ,   7 ,   7 ,
   7 ,   9 ,   6 ,   6 ,   6 ,   11,   6 ,   3 ,   8 ,   7 ,
   11,   13,   8 ,   6 ,   14,   11,   13,   13,   10,   11,
   12,   11,   16,   11,   9 ,   23,   12,   10,   19,   11,
   10,   19,   14,   5 ,   10,   11,   14,   15,   11,   11,
   8 ,   6 ,   4 ,   6 ,   7 ,   6 ,   5 ,   7 ,   9 ,   7 ,
   10,   7 ,   8 ,   10,   4 ,   7 ,   10,   6 ,   7 ,   7 ,
   14,   11,   18,   14,   13,   22,   17,   16,   12,   11,
   20,   16,   16,   15,   18,   16,   20,   22,   14,   19,
   21,   19,   17,   15,   22,   16,   22,   22,   18,   21)

data <- data.frame(as.factor(task),as.factor(age),recall)
eysenck <- aov(recall~task+age+task*age,data=data)
summary(eysenck)

# Simple Effects
simple.eysenck <- aov(recall~age,subset=(task=="Intentional"),data=data)
summary(simple.eysenck) # Note non-standard error term

# Because the subset option in aov doesn't use the MSWithin as the error
# variance estimate, I wrote a little function for you that will do it 
# the standard way.  This function will work for a two-way factorial ANOVA
# design with independent factors "factor1" and "factor2" and dependent 
# variable "variable."
simple.main.effects <- function(variable,factor1,factor2) {
  # Extract MSE and error degrees of freedom from the full model
  model.all <- aov(variable~factor1+factor2+factor1:factor2)
  Error.df <- summary(model.all)[[1]]["Residuals","Df"]
  MSE <- summary(model.all)[[1]]["Residuals","Mean Sq"]
  
  # Extract the names of the factor levels
  levels1 <- unique(factor1)
  levels2 <- unique(factor2)
  
  #### Simple effects ####
  effects1 <- effects2 <- vector()
  
  # Effects of factor2 when factor1 is fixed
  for (i in 1:length(levels1)) {
    model2 <- aov(variable~factor2,subset=(factor1==levels1[i]))
    effects2[i] <- summary(model2)[[1]]["factor2","Mean Sq"]
  }
  # Compute the F statistics and p-values for each level of factor1
  F1 <- effects2/MSE
  p1 <- 1 - pf(F1,length(levels1)-1,Error.df)
  
  # Effects of factor1 when factor2 is fixed
  for (i in 1:length(levels2)) {
    model1 <- aov(variable~factor1,subset=(factor2==levels2[i]))
    effects1[i] <- summary(model1)[[1]]["factor1","Mean Sq"]
  }
# Compute the F statistics and p-values for each level of factor2
  F2 <- effects1/MSE  
  p2 <- 1 - pf(F2,length(levels2)-1,Error.df)

# Return the results.  Don't forget to adjust your alpha level for
# multiple comparisons.
  return(list("factor2"=cbind(levels1,F1,p1),
              "factor1"=cbind(levels2,F2,p2)))
  }

simple.main.effects(recall,task,age)


### Effect sizes
library("effectsize")
eta_squared(eysenck,partial=FALSE)
omega_squared(eysenck,partial=FALSE)

### FAS in baby rats data
trials <- c(
  5,4,3,4,2,
  6,7,5,8,4,
  18,19,14,12,15,6,9,5,9,3)
days <- rep(c(rep(c(5,15),each=5)),times=2)
etoh <- rep(c(0,35),each=10)
data <- data.frame("trials"=trials,"days"=as.factor(days),
              "etoh"=as.factor(etoh))


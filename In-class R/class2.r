# My data are in comma separated value (csv) format, with the first line (header) naming the variables:
data <- read.csv("GSS_1980.csv",header=TRUE)

# Take a peek at the first few lines of the dataset:
head(data)
# We don't need the respondent ID number so we'll delete it:
data$X <- NULL
head(data)
# Give the variables in the dataframe nice names (be careful about order)
names(data) <- c("income","sibs","age","happy","health","taught")

# Note that the $ character lets you pull out a variable from a data frame:
table(data$health) # frequency distribution
table(data$health)/length(data$health) # relative freq distribution

# Make nice bar charts
library('ggplot2') # Load the ggplot2 library

# Frequency histogram
ggplot(data=data,aes(x=health)) + geom_histogram(binwidth=1,color="black",fill="white") + 
  labs(title="Frequency Distribution", y="Frequency", x="Health Rating")  +
  theme(panel.border=element_rect(color="black",fill=NA))

# Relative frequency histogram
ggplot(data=data,aes(x=health)) + 
  geom_histogram(aes(y=..density..),binwidth=1,color="black",fill="white") + 
  labs(title="Relative Frequency Distribution", 
       y="Proportion", x="Health Rating") +
  theme(panel.border=element_rect(color="black",fill=NA))

# Relative frequency distribution of age rounded to 2 significant digits
p <- table(data$age)/length(data$age)
# cumulative frequency distribution rounded to 2 significant digits
P <- cumsum(table(data$age))/length(data$age) 

# Plot the cumulative relative frequency distribution
ages <- data.frame(age=as.numeric(names(P)),proportion=P)
ggplot(ages, aes(x = age, y = proportion)) + 
  geom_step(linewidth = .75, direction = "hv") +
  geom_point(size = .25) + 
  labs(title = "Cumulative Relative Frequency Distribution",
       x = "Age (Years)",
       y = "Proportion") 

# Compute quantiles
median(data$age)
quantile(data$age,seq(0,1,by=.05)) # Vary "by=" for other quantiles

# Central tendency
library('DescTools') # Has some useful stuff
mean(data$health)
median(data$health)
Mode(data$health) # Mode function from DescTools package)

# Weighted mean
p.Health <- table(data$health)
Health <- as.numeric(names(p.Health))
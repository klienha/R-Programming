#Read the file vehicles.csv
car <- read.csv('vehicles.csv')
car
#Create a histogram of Gastax
help(hist)
hist(car$Gastax)
##Alter the scatter plot (title and label it), 3 classes
hist(car$Gastax, col ="Dark Grey", main="Histogram of Gas Taxes Across the United States",
     xlab="Tax on Gasoline (Cents/Gallon)", ylab="Frequency of States",col.lab="Dark Blue",
     breaks = 3)
##Alter the scatter plot (title and label it), 25 classes
hist(car$Gastax, col ="Dark Grey", main="Histogram of Gas Taxes Across the United States",
     xlab="Tax on Gasoline (Cents/Gallon)", ylab="Frequency of States",col.lab="Dark Blue",
     breaks = 25)
###Calculate statistics of License Age
##Find the mode
lisage <- car$LicAge
lisage
#sort the variables
sort(lisage)
#visualize LicAge
hist(car$LicAge, 20)
##Find Median
median(lisage, na.rm=TRUE)
##Find Mean
mean <- mean(lisage, na.rm=TRUE)
mean
##Find Range
range <- max(lisage) - min(lisage)
range
##Find Standard Deviation
stdv <- sd(lisage)
stdv
##Find IQR
inqura <- IQR(lisage)
inqura
##Find Coefficient of Variation
myCV <- sd(lisage) / mean(lisage) #Relative dispersion around the mean
myCV
###Install packages for more functions
install.packages("moments")
library("moments")
##Find Skewness
skewness(lisage)
##Find Kurtosis
kurtosis(lisage)

###Calculate statistics of GasTax
##Find the mode
gastax <- car$Gastax
gastax
#sort the variables
sort(gastax)
#visualize LicAge
hist(car$Gastax, 20)
##Find Median
median(gastax, na.rm=TRUE)
##Find Mean
mean1 <- mean(gastax, na.rm=TRUE)
mean1
##Find Range
range1 <- max(gastax) - min(gastax)
range1
##Find Standard Deviation
stdv1 <- sd(gastax)
stdv1
##Find IQR
inqura1 <- IQR(gastax)
inqura1
##Find Coefficient of Variation
myCV1 <- sd(gastax) / mean(gastax) #Relative dispersion around the mean
myCV1
###Install packages for more functions
install.packages("moments")
library("moments")
##Find Skewness
skewness(gastax)
##Find Kurtosis
kurtosis(gastax)

###Creates boxplots of LicAge and GasTax
boxplot(lisage, col= "Gold", main="Boxplot of License Age", ylab="Age of First License")
boxplot(gastax, col= "Dark Red", main="Boxplot of Gas Taxes", ylab="Tax on Gasoline (Cents/Gallon)")

###Built-in data set calculations
st <- data.frame(state.x77)
st
##Calculate average state population without using the mean function
pop <- st$Population
pop
nm <- length(pop)
meanpop <- sum(pop)/nm
meanpop
##Calculate standard deviation of state population without using sd() or var()
xbar <- mean(pop)
stdev <- sqrt(sum((pop-xbar)^2)/(nm-1))
stdev
##Calculate weighted mean of life expectancy; weights are population of states
le <- st$Life.Exp
le
wm <- weighted.mean(le, pop)
wm




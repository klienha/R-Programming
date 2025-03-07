#Read the counties file
counties <- read.csv("counties.csv")
counties
#Set counties DIVINDX10 to its own variable
diverse <- (counties$DIVINDX10)
diverse
#Find mode
modediv <- sort(diverse)
myMode <- function(modediv) {
  z <- table(as.vector(modediv))
  names(z)[z == max(z)]
}
myMode(diverse)
#Find the median of counties$DIVINDX10
meddiv <- median(diverse, na.rm=TRUE)
meddiv
#Find the mean of counties$DIVINDX10
meandiv <- mean(diverse)
meandiv
#Find the standard deviation of counties$DIVINDX10
stdvdiv <- sd(diverse)
stdvdiv
#Find the kurtosis of counties$DIVINDX10
library("moments")
kurtdiv <- kurtosis(diverse)
#Find the skewness of counties$DIVINDX10
skewdiv <- skewness(diverse)
skewdiv
#Create a histogram of counties$DIVINDX10
hist(diverse, plot=TRUE, main="Histogram of Diversity Index", col = "Light Blue",
     xlab="Percentage Chance", ylab="Frequency",xlim = c(0,90))
#Create a distribution of sample means for the D.I, sample size = 5. 100,000 samples
sam <- vector()

for(i in seq(1,100000)) {
  sam[i] <- mean(sample(diverse, 5))
}
hist(sam, main="Histogram of Diversity Index", col = "Orange",
     xlab="Percentage Chance", ylab="Frequency", xlim = c(0,90))

##Create a distribution of sample means for the D.I, sample size = 50. 100,000 samples
sam2 <- vector()

for(i in seq(1,100000)) {
  sam2[i] <- mean(sample(diverse, 50))
}
hist(sam2, main="Histogram of Diversity Index", col = "Violet",
     xlab="Percentage Chance", ylab="Frequency", xlim = c(0,90)) 
#Standard deviation of both sample distributions
sd(sam)
sd(sam2)
#Standard error of the mean for both sample distributions
stdvdiv/sqrt(5)
stdvdiv/sqrt(50)
#Calculate a 90% CI for mean commute time, 120 residents, avg. time 28.4 min, stdv 15.1
meantim <- 28.4
zscore <- 1.65
stdvtim <- 15.1
sampleres <- 120
#C.I. boundaries
confinup <- meantim + zscore * stdvtim/sqrt(sampleres)
confinup
confindown <- meantim - zscore * stdvtim/sqrt(sampleres)
confindown
#Calculate a 95% CI around estimate, if prop. > 10% underwater means further study, 39 of 250 homeowners underwater
prop <- 39/250
qprop <- 1 - prop
zscore1 <- 1.96
sampleun <- 250
#C.I. boundaries
confinupw <- prop + zscore1 * ((sqrt(prop*qprop))/(sqrt(sampleun-1)))
confinupw
confindownw <- prop - zscore1 * ((sqrt(prop*qprop))/(sqrt(sampleun-1)))
confindownw
#Calculate a 95% CI, sample 10 plots, estimate stdv 50 Mg/ha, within 10 Mg/ha of the true mean, how many plots?
zscore2 <- 1.96
stdvplot <- 50
within <- 10
numplot <- (zscore2**2 * stdvplot**2) / (within**2)
numplot
sampleplot <- 10
plots <- numplot - sampleplot
plots





#Determine alpha for different confidence intervals
#Determine significance for confidence interval 0.90, 0.95, 0.99
CI90 <- 0.90
CI95 <- 0.95
CI99 <- 0.99
alpha90 <- 1 - CI90
alpha95 <- 1 - CI95
alpha99 <- 1 - CI99
#Find 1-tailed quantiles of a normal distribution based on significance
qnorm(alpha90)
qnorm(alpha95)
qnorm(alpha99)
##Find 2-tailed quantiles of a normal distribution based on significance/2
qnorm(alpha90/2)
qnorm(alpha95/2)
qnorm(alpha99/2) 
#Does Big Rapids have a stronger religious attendance than average American comm.
##Amper= 0.43, Bigper= 44/86, CI = 0.95; right-tailed one-tailed z-distribution
zorig <- qnorm(0.05, lower.tail=FALSE)
Amper <- 0.43
Bigper <- (44/86)
qBig <- 1 - Bigper
znum<- (Bigper - Amper)
zden <- sqrt((Bigper*qBig)/86)
z <- znum/zden
#Does the farmer produce more apples than average.
##Avapp= 40, Farmapp= 42, sampled = 25, sd = 6, CI = 0.95; right-tailed one-tailed t-distribution
zorig1 <- qt(0.05, 24, lower.tail=FALSE)
Avapp <- 40
Farmapp <- 42
sd <- 6
sampletree <- 25
znum1 <- (Farmapp - Avapp) 
zdenom1 <- (sd / sqrt(sampletree))
zapp <- znum1/zdenom1
#Check answers using built-in functions
#2a z-test CI = 0.95
np <- 86 #sample size
xp <- 44 #number religious attendance
sp <- xp / np #get sample proportion
rho <- Amper #population proportion
help(prop.test)
prop.test(xp, np, p=rho, alternative="greater", conf.level=.95, 
          correct=F)
testz <- sqrt(2.3379)
testz
#2b t-test
install.packages("PASWR")
require(PASWR)
ns <- 25 #sample size of smaller sample
xs <- 42 # mean of smaller sample
ss <- 6 #sd of smaller sample
mu <- 40 #Ho
tsum.test(mean.x=xs, s.x=ss, n.x=ns, mu=mu, alternative="greater", 
          conf.level=.95)





#Read US cities file
uscities <- read.csv('uscities.csv')
#Assign population values to variables
pop2010 <- uscities$pop2010
pop2000 <- uscities$pop2000
#Histogram of difference in pop between 2010 and 2000
diffpop <- pop2010 - pop2000
diffpop
hist(diffpop, main="Histogram of Difference in Population between 2010 and 2000 
     in US Top 40 Largest Cities",
     xlab = "Population Difference", col = "maroon", breaks = 25)
#Add abline at the average of the difference in pop histogram
abline(v = mean(diffpop), col="blue")
#Population Testing
#Parametric Test: Matched-Pair T-test (testing pop2010 > pop2000)
t.test(pop2010,pop2000,alternative="greater", paired=T, conf.level=.95)
#Nonparametric Test: Wilcoxon matched-pairs signed-ranks test (testing pop2010 > pop2000)
wilcox.test(pop2010, pop2000, paired=T, alternative = "greater", conf.level=.95)
#Farm County Testing
loudfarmx <- 43
loudfarmn <- 123
princefarmx <- 39
princefarmn <- 101
#Vector of values
farmsx <-c(princefarmx,loudfarmx)
farmsn<-c(princefarmn,loudfarmn)
#Parametric Test: Proportion T-test (testing princefarm > loudfarm)
prop.test(farmsx, farmsn, alternative="greater", conf.level=.95, correct=F)
zscore <- sqrt(0.31918)
#Read Airpoll file
airpoll <- read.csv('airpoll.csv')
airpoll
#Save the northparticulates and southparticulates in variables
northpart <- airpoll$Particulate[airpoll$Region=="N"]
southpart <- airpoll$Particulate[airpoll$Region=="S"]
#Parametric Test: Two-sample difference of means (testing northpart < southpart)
t.test(northpart, southpart, alternative="less", conf.level=.95)
#Nonparametric Test: Wilcoxon rank sum test (testing northpart < southpart)
wilcox.test(northpart, southpart, alternative="less", conf.level=.95)


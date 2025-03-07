#Read file
carbon <- read.csv('carbon data.txt')
#Area Samples
Yosemite <- c(19.2,18.7,21.3,16.5,17.3,22.4)
Sequoia <- c(18.7,14.3,20.2,17.6,19.3,16.1)
Tahoe <- c(12.5,14.3,8.7,11.4,9.5,16.5)
Joshua <- c(20.3,22.5,17.6,18.4,15.9,19.0)
Shasta <- c(19.9,25.3,17.6,20.2,18.4,19.1)
#Data Frame
area <- c(Yosemite,Sequoia,Tahoe,Joshua,Shasta)
group <- c(rep("1",6),rep("2",6),rep("3",6),rep("4",6),rep("5",6)) 
frame1 <- data.frame(area, group)
frame1
#Area Carbon Boxplot
boxplot(Yosemite,Sequoia,Tahoe,Joshua,Shasta, 
        names=c("Yosemite","Sequoia","Tahoe","Joshua","Shasta"),xlab="Areas",
        ylab="Carboniferous Sand Percentage",main="Boxplot of Carboniferous Sand Percentage by Area",
        col ="Tan")
#Area Carbon Boxplot with Notches
boxplot(Yosemite,Sequoia,Tahoe,Joshua,Shasta, 
        names=c("Yosemite","Sequoia","Tahoe","Joshua","Shasta"),
        notch = T, xlab="Areas", ylab="Carboniferous Sand Percentage",
        main="Boxplot of Carboniferous Sand Percentage by Area", col ="Tan")
#ANOVA parametric test
oneway.test(area ~ group, data = frame1, var=T)
summary(aov(area ~ group, data = frame1))
#K-W nonparametric test
kruskal.test(frame1$area, frame1$group)
#Means of Areas
xYosemite <- mean(Yosemite)
xSequoia <- mean(Sequoia)
xTahoe <- mean(Tahoe)
xJoshua <- mean(Joshua)
xShasta <- mean(Shasta)
#Standard Deviation of Areas
sdYosemite <- sd(Yosemite)
sdSequoia <- sd(Sequoia)
sdTahoe <- sd(Tahoe)
sdJoshua <- sd(Joshua)
sdShasta <- sd(Shasta)
#Data frame Tahoe & Shasta
areatahsha <- c(Tahoe,Shasta)
grouptahsha <- c(rep("1",6),rep("2",6))
frametahsha <- data.frame(areatahsha, grouptahsha)
#ANOVA parametric test Tahoe & Shasta
oneway.test(areatahsha ~ grouptahsha, data = frametahsha, var=T)
summary(aov(areatahsha ~ grouptahsha, data = frametahsha))
#Data frame Joshua & Yosemite
areajoshyos <- c(Joshua, Yosemite)
groupjoshyos <- c(rep("1",6),rep("2",6))
framejoshyos <- data.frame(areajoshyos, groupjoshyos)
#ANOVA parametric test Joshua & Yosemite
oneway.test(areajoshyos ~ groupjoshyos, data = framejoshyos, var=T)
summary(aov(areajoshyos ~ groupjoshyos, data = framejoshyos))
#Music Table Stats
alln <- c(6,6,6) # all of the number of samples
allx <- c(823,786,842) # all of the sample means
allsd <- c(23.723, 20.919, 27.055)
N <- sum(alln) # sum of all observations in the sample
k <-  length(alln)
#The between-group mean squares (MSb)
MSb <- 9732/ (k - 1)
MSb
#Degrees of freedom between-group
df1 <- k-1
#The within-groups mean squares 
MSw <- 8662 / (N - k)
MSw
#Degrees of freedom within-group
df2 <- N - k
#Calculating the F stat
Fstat <- MSb / MSw
Fstat
#P-value
pf(Fstat, 2, 15,low=F)



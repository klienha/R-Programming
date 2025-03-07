#Read starbucks data
starbucks <- read.csv("starbucks.csv")
starbucks
#Assign X (number of Starbucks) an Y (Starbucks GSP per capita)
x <- starbucks$gsp_pcap
y <- starbucks$starbucks
#y = a + bx
#Start with a scatterplot
plot(x,y, main = "Starbucks GSP per Capita & Number of Starbucks",
     xlab = "Starbucks GSP per Capita", ylab = "Number of Starbucks", col ="blue")
# Finding a and b "by hand"
b <- cor(x,y) * (sd(y) / sd(x))
b
a <- mean(y) - b * mean(x)
a
#Plot the regression line over the scatterplot
abline(a, b)
#Test the coefficient of determination to see how much of the 
# variation depends on the explained component
n = length(x)
#RSS - regression sum of squares
rss <- (((sum(x*y))-((sum(x)*sum(y)) / n))^2) / ((sum(x^2))-((sum(x)^2) / n))
rss
#TSS - total sum of squares
tss <- sum((y-mean(y))^2)
tss
#Get the coefficient of determination (r^2)
rsq <- rss/tss
rsq
#Get the F statistic from ANOVA (expressed in terms of r)
f <- (rsq * (n-2)) / (1-rsq)
f
#No. of explanatory variables (called numerator df)
df1 = 1 
#No. of parameters in regression equation (called denominator df)
df2 = n - 2 
#The p-value for the F test is always a right-tailed test
pvalue <- pf(f, df1, df2, low=F)   
pvalue
#Bivariate Linear Regression
reg <- lm(y ~ x)
reg
summary(reg)
#Multivariate Linear Regression
birth <- starbucks$birthrate
median <- starbucks$med.age
totalmur <- starbucks$totmurder
#Use the "linear model" command and assign it to a variable
#Starbucks number of stores dependent, 4 other variable independent
reg2 <- lm(y ~ x + birth + median + totalmur)
reg2
summary(reg2)
# view standardized residual plots
# Best to look at residuals vs each independent variable
#gsp
plot(x, rstandard(reg2),main="Residuals vs GSP",ylab="Standardized Residuals",
     xlab="GSP", abline(h=0))
#birth
plot(birth, rstandard(reg2),main="Residuals vs Birth",ylab="Standardized Residuals",
     xlab="Birth", abline(h=0))
#median
plot(median, rstandard(reg2),main="Residuals vs Median",ylab="Standardized Residuals",
     xlab="Median", abline(h=0))
#total murders
plot(totalmur, rstandard(reg2),main="Residuals vs Total Murder",ylab="Standardized Residuals",
     xlab="Total Murders", abline(h=0))
# But also, can look at the fitted values
plot(fitted(reg2), rstandard(reg2),main="Residual Plot",ylab="Standardized Residuals",
     xlab="Fitted Values", abline(h=0))
# and QQplot
qqnorm(rstandard(reg2),main="Normal Q-Q Plot",ylab="Sample Quantiles",
       xlab="Theoretical Quantiles")
qqline(rstandard(reg2))
#Bivariate Linear Regression Murder
reg3 <- lm(y ~ totalmur)
reg3
summary(reg3)


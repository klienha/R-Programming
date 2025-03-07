#Read file
human <- read.csv('human_development.txt')
human
#Assign variables
gdp <- c(6.09,11.32,25.37,26.73,25.52,7.36,27.13,9.19,4.02,29,3.52,24.43,23.99,
         25.35,17.44,2.84,6,32.41,19.79,25.13,8.75,8.43,27.19,19.16,0.85,29.62,
         1.89,3.84,7.1,13.33,11.29,20.15,24.18,28.1,5.89,24.16,34.32,2.07,0.79)
co2 <- c(3,3.8,18.2,7.6,10.2,1.8,14.4,4.2,2.3,9.3,2,11.3,6.1,9.7,8.2,1.1,4.8,10.8,
         10,9.1,5.4,3.9,8.5,8.1,0.3,8.7,0.7,1,9.8,11.7,7.9,6.8,5.3,5.7,3.1,9.2,19.7,
         0.6,1.1)
plot(gdp, co2, col="blue", xlab="Gross Domestic Product", ylab="CO2 Emissions",
     main="Gross Domestic Product vs CO2 Emissions")
# Rho
# Calculate rho to see whether or not there is a correlation between x and y.
cor.test(gdp, co2)
# Spearman's r correlation
# Convert gdp and co2 to ordinal data to calculate
# Test rho to see whether or not there is a correlation between x and y. 
cor.test(rank(gdp), rank(co2), m ="s") 
cor.test(rank(gdp), rank(co2), m ="s", alternative = "greater")
cor.test(rank(gdp), rank(co2), m ="s", alternative = "less")
#Shapiro-Wilk test to see if a sample is normal.
# Let's test gdp and co2
shapiro.test(gdp)
hist(gdp)

shapiro.test(co2)
hist(co2)
#Chi-square Goodness-of-Fit Test 
dc <- c(26,10,14)
chisq.test(dc)
#Contingency analysis 
md <- c(28,9,7)
de <- c(25,6,4)
va <- c(21,6,4)
wv <- c(29,4,2)
degree <- data.frame(dc,md,de,va,wv)
# How long are the rows and columns?
nrows <- nrow(degree) #total rows
ncols <- ncol(degree) #total columns

# Get marginal sums (row and column sums)
rowsum <- apply(degree, 1, sum) #1 means sum horizontally
colsum <- apply(degree, 2, sum) #2 means sum vertically

# What is our total N?
N <- sum(rowsum)
# Also can do N <- sum(colsum)

# Expected frequencies - use outer() to do matrix multiplication
expect <- outer(rowsum, colsum) / N

# Chi-square stat
cs <- sum((degree - expect) ^ 2 / expect)
cs

# P value
df <- (nrows - 1) * (ncols - 1)
p <- pchisq(cs, df, low=F)
p






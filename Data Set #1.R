#Read the file states.csv
states<- read.csv('states.csv')
states
#Create a scatter plot of Illiteracy vs. HS Grad
help(plot)
plot(states$Illiteracy, states$HS.Grad)
##Alter the scatter plot (title and label it)
plot(states$Illiteracy, states$HS.Grad, col="Black", 
     main="Illiteracy Rate vs HS Graduation Rate by State",col.main="Maroon",
     xlab="Illiteracy Rate (%)", ylab="HS Graduation Rate (%)",col.lab="Dark Blue")
#Create a histogram of Life Expectancy 
help(hist)
hist(states$Life.Exp)
##Alter the histogram (title and label it) and bin Life Exp. by each whole year
hist(states$Life.Exp, col="light grey", main="Average Life Expenctancy in the
     United States", xlab="Average Age of Death In Years", ylab="Frequency in 
     Number of States",col.lab="Maroon", breaks=c(67,68,69,70,71,72,73,74))
#Data from another file
#Read the file precip.csv
precip<- read.csv('precip.csv')
precip
#Create a scatter plot of precipitation in Baltimore vs. DC 
help(plot)
plot(precip$bal, precip$dc)
##Alter the scatter plot (title and label it)
plot(precip$bal, precip$dc, col="Medium Blue", main="Yearly Precipitation in Baltimore vs. DC",
     xlab="Precipitation in Baltimore in Inches", ylab="Precipitation in DC in Inches",
     col.lab="Dark Blue")
#Calculate the difference in yearly precipitation between Baltimore and DC
#Positive values show years when Washington had more rain. Negative values show years when Baltimore had more rain.
rain<- (precip$dc - precip$bal)
rain
#Create a vector for the years of rain recorded
time<- (precip$year)
time
#Create a dataframe with the years of rain recorded and the rain difference between cities
datarain <- data.frame(year= time, difference= rain) 
datarain
#What was the biggest lead Washington had over Baltimore (year of biggest positive value)
topwash<- max(datarain$difference)
#What was the biggest lead Baltimore had over Washington (year of biggest negative value)
topbal<- min(datarain$difference)
#Create a histogram of the rain differences
help(hist)
hist(datarain$difference)
##Alter the histogram (title and label it) and change breaks
hist(datarain$difference, col="Light Blue", main="Rain Differences Between Baltimore and DC",
  xlab="Inches of Rain Differences (Baltimore (-) and DC (+))", 
  ylab="Frequency of Yearly Rain Differences Occuring",col.lab="Medium Blue",
  breaks= -12:10)


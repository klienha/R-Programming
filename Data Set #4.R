# What is the probability that both will arrive on time?
# p (me on time) = 0.50
# p(friend on time) = 0.40
# p(both On time) = p(me on time) AND p(friend on time) # note 'AND'!
pme <- 0.50; pfr <- 0.40
pme * pfr
# p(me On time and friend late) = p(me on time) AND p(friend late) # note 'AND'!
pme * (1-pfr)
# p(one On time) = p(me on time) OR p(friend on time) # note 'OR'!
pme + pfr - (pme * 1-pfr)
#p 4 days >= 100 degrees with average 1.6 days >= 100 degrees per year in dc
dchot <- (1.6**4)/ ((exp(1.6))*(factorial(4)))
dchot
#p 4 dc snow days in January (31 days) with a snowy particular day being p 0.08
dcsnow <- (factorial(31) * (0.08 ** 4) * ((1-0.08)**(31-4)))/ ((factorial(4) *
  (factorial(31-4))))
dcsnow
#p < 10 KW of energy in any particular day in July, stdv = 5.4, mean = 15.3 KW
enerjul <- (10 - 15.3)/(5.4)
enerjul
#read precip.csv
precip <- read.csv('precipitation.csv')
precip
#get standard score
#mean
precipmean <- mean(precip$dc)
#observation
precipob <- 65
#standard deviation
precipstdv <- sd(precip$dc)
#standard score
zi <- (precipob -precipmean)/ precipstdv
zi 
#find probablity any given year gets over 65 inches of rain
help(pnorm)
pnorm(zi)
1-pnorm(zi)
#read buildings.csv
building <- read.csv('buildings.csv')
building
probzero = (1 - (3/122))** 10
#poisson distribution of p >0 squirrels nest
help(dpois)
nestamount <- c(0, 1, 2, 3, 4)
nestfrequency <- c(42, 39, 13, 3, 1)
nest <- data.frame(nestamount, nestfrequency)
nest
meannest <- (0*42 + 1*39 + 2*13 + 3*3 + 4*1)/(42 + 39 + 13 + 3 + 1) 1 
pnest <- ((exp(-0.79591836))* 0.79591836**0) / factorial(0)
1-pnest



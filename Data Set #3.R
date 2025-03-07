# Read earthquake data
help(read.csv)
quake <- read.csv("earthquakes.csv")
quake
# Earthquake mean center in decimal degrees
mlatquake <- mean(quake$X)
mlongquake <- mean(quake$Y)
mlatquake; mlongquake
# Read church data
help(read.csv)
mcchurch <- read.csv("mc.csv")
pgchurch <- read.csv("pg.csv")
# Mean center of Churches for each county
#Mean center Montgomery
mclat <- mean(mcchurch$XCoord)
mclong <- mean(mcchurch$YCoord)
mclat; mclong
#Mean center Prince George's
pglat <- mean(pgchurch$XCoord)
pglong <- mean(pgchurch$YCoord)
pglat; pglong
# Distance between mean center and county centroid in each county
#Montgomery county centroid
mccentlat <- 1254424.75167
mccentlong <- 535309.382717
#Euclidean distance Montgomery
edist <- sqrt((mclong - mccentlong)^2 + (mclat - mccentlat)^2)
edist
#Manhattan Distance Montgomery
mdist <- abs(mclong - mccentlong) + abs(mclat - mccentlat)
mdist
#Prince George county centroid
pgcentlat <- 1356774.29008
pgcentlong <- 424327.931452
#Euclidean distance Prince George
edist1 <- sqrt((pglong - pgcentlong)^2 + (pglat - pgcentlat)^2)
edist1 
#Manhattan Distance Prince George
mdist1 <- abs(pglong - pgcentlong) + abs(pglat - pgcentlat)
mdist1
# Standard Distance of churches in each county
#Standard distance Montgomery
xmc <- sum((mcchurch$YCoord - mclong)^2)
ymc <- sum((mcchurch$XCoord - mclat)^2)

sdmc <- sqrt((xmc+ymc)/length(mcchurch$YCoord))
sdmc
#Standard distance Prince George
#Standard distance Montgomery
xmc1 <- sum((pgchurch$YCoord - pglong)^2)
ymc1 <- sum((pgchurch$XCoord - pglat)^2)

sdmc1 <- sqrt((xmc1+ymc1)/length(pgchurch$YCoord))
sdmc1

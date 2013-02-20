# Creator: Ricco Amezcua
# Created on: Feb 10, 2013
# Class: CS422
# This script imports the red and white data sets and merges them.
# It then creates a histogram with 6 bins and one with 2 bins.
# Then is samples the first 50 entries of red and first 50 entries of
# white wine and creates a parallel coordinated plot of the
# citric acid, residual sugar, density, and quality.

redwine <- read.table("winequality-red.csv", sep=";", header=T)
whitewine <- read.table("winequality-white.csv", sep=";", header=T)
allwine <- rbind(redwine, whitewine)
attach(allwine)

# Histogram with all frequencies
hist(quality, main="Histogram of Wine Quality", xlab="Quality", col="chocolate")

# Histogram with two bins
hist(quality, breaks=2,main="Histogram of Wine Quality (Two Bins)", xlab="Quality",col="royalblue")

detach(allwine)

#Create parallel coordinates plot
fiftyRed <- redwine[1:50,]
fiftyWhite <-whitewine[1:50,]
allWineSample <- rbind(fiftyRed, fiftyWhite)

library("lattice")
attach(allWineSample)
parallelplot(allWineSample[c('citric.acid', 'residual.sugar', 'density', 'quality')], main="Sample of White and Red Wine", varnames=c("Citric Acid", "Residual Sugar","Density","Quality"), xlab="Wine Properties", ylab="Measurement", horizontal.axis = F)
detach(allWineSample)
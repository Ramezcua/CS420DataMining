# Creator: Ricco Amezcua
# Created on: Feb 14, 2013
# Class: CS422
# This file contains the commands for creating the joint histogram and the colored 2d plot

adult <- read.table("adult.data", sep=",", header=F)
attach(adult)

# Joint Histogram for Marital Status and Education Level
dotchart(table(adult$V4, adult$V6), main = "Marital Status and Education Level", xlab = "Frequency", pch = 4, col= "royalblue4",cex=0.4)

# The ggplot 2 library is need 
library(ggplot2)

# Making a new data frame for the education number, hours per week, and age
adultprop <- adult[,c("V5", "V13", "V1")]
names(adultprop) <- c("Education Number", "Hours", "Age")

# A rainbow gradient is used because it gives a lot of variation to the ages
ggplot(adultprop, aes(x=Education_Number, y=Hours, colour = Age)) +geom_point() + ggtitle("Education Number vs Hours Per Week vs Age") +scale_colour_gradientn(colours = rainbow(100))

detach(adult)
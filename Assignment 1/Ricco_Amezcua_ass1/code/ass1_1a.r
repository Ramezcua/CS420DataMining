# Creator: Ricco Amezcua
# Created on: Feb 10, 2013
# Class: CS422
# This file contains the code for assignment 1, question 1, part a.
# It imports data on red wine, seperates them into high quality and low quality
# and gives the mean and standard deviation for all of the wine's properties.  
# It also creates a scatter plot matrix for the residual sugar, total sulfur 
# dioxide, and alchol.
# This script outputs 4 files in a folder called data in the working directory:
# 	  redHQ_Mean.txt, redHQ_StandDeviation.txt, redLQ_Mean.txt, and redLQ_standDeviation.txt

redwine <- read.table("winequality-red.csv", sep=";", header=T)
attach(redwine)
redLowQuality <- redwine[which(quality<=5),]
redHighQuality <- redwine[which(quality>5),]
detach(redwine)

sink("./data/redLQ_Mean.txt", split=T)
sapply(redLowQuality,mean)
sink()

sink("./data/redHQ_Mean.txt", split=T)
sapply(redHighQuality,mean)
sink()

sink("./data/redLQ_StandDeviation.txt", split=T)
sapply(redLowQuality,sd)
sink()

sink("./data/redHQ_StandDeviation.txt", split=T)
sapply(redHighQuality,sd)
sink()

#(No longer used) Scatter Plot Matrix for Low Quality Wine
#pairs(~residual.sugar+total.sulfur.dioxide+alcohol, data=redLowQuality, main = "Low Quality Redwine",col="red", labels=c("Residual Sugar","Total Sulfur Dioxide","Alchohol"))

# (No longer used) Scatter Plot Matrix for High Quality Wine
#pairs(~residual.sugar+total.sulfur.dioxide+alcohol, data=redHighQuality, main = "High Quality Redwine",col="blue", labels=c("Residual Sugar","Total Sulfur Dioxide","Alchohol"))

# Creation of a new attribute in redwine that categorizes into high and low quality wine
redwine$qualitycat <- ifelse(redwine$quality > 5, "HighQuality", "LowQuality")


# Scatter Plot Matrix for High and Low quality (has the both data sets on one plot)
pairs(redwine[,c("residual.sugar", "total.sulfur.dioxide", "alcohol")], main = "Wine: High Quality vs Low Quality", labels=c("Residual Sugar","Total Sulfur Dioxide","Alchohol"), pch = 21, bg = c("blue","red")[as.factor(redwine$qualitycat)])
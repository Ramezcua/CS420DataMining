# Creator: Ricco Amezcua
# Created on: Feb 14, 2013
# Class: CS422
# This file contains the commands for creating the boxplots and 3D scatter plot

adult <- read.table("adult.data", sep=",", header=F)
attach(adult)

# Histogram for the race count in the United States
# Need to create a americans variable first
americans <- adult[which(V14==" United-States"),]
barplot(table(americans$V9), main="Races in the United States", names.arg=c("Ind/Esk", "Asn/Isl", "Black","Other", "White"), xlab="Race", ylab="Count")

# Boxplot for Education Number
boxplot(V5, main="Education Number", ylab="Scale")

# Boxplot for Capital Gain
boxplot(V11, main="Capital Gain", ylab="Scale") ?dot

#Boxplot for Hours-Per-Week
boxplot(V13, main="Hours-Per-Week", ylab="Scale") 

# 3D scatterplot for workclass, race, and hours per week
scatterplot3d(data.frame(as.integer(adult$V2), as.integer(adult$V9), adult$V13), main = "Race and Job", xlab = "Workclass", ylab = "Race", zlab = "Hours-Per-Week", pch = 4, color = "royalblue2")

detach(adult)
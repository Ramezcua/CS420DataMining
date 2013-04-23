# Created by Ricco Amezcua
# First created on 4/22/2013
# Last updated on 4/22/2013
# This file contains the assignment code Assignment 5 for CS 422
# It uses the iris data set

#### Constants ####
# This is the difference threshold for the stopping condition
# It is a percentage of difference for two clusters
# When the percentage of difference falls below or is the threhold
# the algorithm will stop
DIFF.THRESH = 0 

#### Functions ####
# This function is used to normal data in a data frame
# It takes a data frame and the number of attributes in that data frame
NormalizeData <- function(df, natt){
  for(i in 1:natt){
    stan <- sd(df[,i])
    mean <- mean(df[,i])
    df[,i] <- sapply(df[,i], FUN = function(x){
      return ((x-mean)/stan)}) # nvm (Added a rounding function here round(x, 3))
  }
  return (df)
}

# This function will update the centroids
# It takes the dataframe of the data, a dataframe with the centroids data
# and the k
UpdateCentroids <- function(df, centroids, k){
  for(i in 1:k){
    temp <- subset(df, Cluster == i)
    temp$Cluster <- NULL # Remove clusters
    centroids[i,]<-as.vector(colMeans(temp))
  }
  
  return (centroids)
}

# This function will find the Euclidean distance between two points
EuclideanDistance <- function(p1, p2){
  distance <- p1-p2
  distance <- sapply(distance, FUN=function(x){return(x^2)})
  distance <- sum(distance)
  distance <- sqrt(distance)
  return (distance)
}

# This function takes a point and the centroids data frame
# It will find the centroid closest to that point (returns the index)
FindCluster <- function(point, centroids){
  k <- nrow(centroids)
  distances <- NULL
  for(i in 1:k){
    distances <- append(distances, EuclideanDistance(point, centroids[i,]))
  }
  #print(distances)
  cluster <- which(distances==(min(distances)))
  return(cluster[1])
}

# This function has the stopping condition for the k - means algo
# It takes two sets of cluster as vectors and finds the difference percentage
# (number of differences)/length
# It also takes a threshold for the difference percentage
StoppingCondition <- function(previous.clusters, current.clusters, threshold){
  stopping.condition <- FALSE
  num.differences <- length(which(previous.clusters != current.clusters))
  # If the number of differences falls below a certain threshold, stop the algo
  difference.percentage <- num.differences/length(current.clusters)
  #print(difference.percentage)
  if (difference.percentage <= threshold){stopping.condition <- TRUE}
  
  return(stopping.condition)
}

# This is my kmeans function.  It takes a data frame of data and a k
# It returns a vector of the clusters associated with the data 
MyKMeans <-function(df, k){
  natt <- ncol(df)
  centroids <- as.data.frame(matrix(data=0, nrow=k, ncol=natt))
  
  # Adding a cluster column and randomly assigning each point to a cluster
  df["Cluster"] <- NA
  df$Cluster <- sample(1:k, nrow(df), replace=TRUE)
  
  stopped <- FALSE
  while(!stopped){
    previous.clusters <- as.vector(df$Cluster)
    
    # remove cluster column for easier calculation
    temp <- df
    temp$Cluster <- NULL
    
    # Update the centroids
    centroids <- UpdateCentroids(df, centroids, k)
    #Update each point's associated cluster
    df$Cluster <- apply(temp, 1,FindCluster, centroids)
    
    current.clusters <- as.vector(df$Cluster)
    
    if (StoppingCondition(previous.clusters, current.clusters, DIFF.THRESH)){
      stopped <- TRUE
    }
  }
  
  return (as.vector(df$Cluster))
}


#### Algo ####

# Creating data
k.three <- iris
k.three$Species <- NULL
k <- 3

# Normalizing the data set
k.three <- NormalizeData(k.three, ncol(k.three))

MyKMeans(k.three, k)

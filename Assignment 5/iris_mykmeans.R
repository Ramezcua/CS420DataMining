# Created by Ricco Amezcua
# First created on 4/22/2013
# Last updated on 4/28/2013
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

# This is a helper function that checks if a set of clusters is in
# the currently active set of clusters.
# It returns a list of inactive clusters, or 0 if all clusters are active
ClusterRepCheck <- function(clusters, active.clusters){
  active.clusters <- unique(active.clusters)
  inactive.clusters <- which(!(clusters %in% active.clusters))
  if(length(inactive.clusters) == 0){inactive.clusters <-0}
  return(inactive.clusters)
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
MyKMeans <-function(df, k, measure.SSE=FALSE){
  natt <- ncol(df)
  centroids <- as.data.frame(matrix(data=0, nrow=k, ncol=natt))
  
  # Adding a cluster column and randomly assigning each point to a cluster
  df["Cluster"] <- NA
  df$Cluster <- sample(1:k, nrow(df), replace=TRUE)
  
  SSE <- NULL
  
  overall.SSE <- NULL
  
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
    
    inactive.clusters <- ClusterRepCheck(c(1:k), current.clusters)
    
    if (inactive.clusters[1] != 0){
      SSEResults <- CentroidSSEVector(df, centroids, inactive.clusters)
      active.clusters <- SSEResults$clusters
      SSEList <- SSEResults$SSE
      
      for(i in inactive.clusters){
        max.index <- which.max(SSEList)
        #Get index of first available point
        candidate.index <- which(df[,"Cluster"] == active.clusters[1])[1] 
        # Give that point to the inactive cluster i
        df[candidate.index, "Cluster"] <- i
        #Remove the used max cluster for the next candidate
        active.clusters <- active.clusters[-(max.index)]
        SSEList <- SSEList[-(max.index)]
      }
      
    }
  
    if (measure.SSE){
      #Measure the SSE and add it to the SSE vector
      current.SSE <- AllClusterSSE(temp, k, as.vector(df$Cluster))
      overall.SSE <- c(overall.SSE, as.numeric(current.SSE))
    }
    
    if (StoppingCondition(previous.clusters, current.clusters, DIFF.THRESH)){
      stopped <- TRUE
    }
  
  }
  
  return (list(Cluster=as.vector(df$Cluster), SSE=overall.SSE))
}



# This function gets the SSE of all active centroids
# This function is used for the redistribution of points
CentroidSSEVector <- function(df, centroids, inactive.clusters){
  active.clusters <- setdiff(unique(df$Cluster), inactive.clusters)
  SSE <- NULL
  for(i in active.clusters){
    temp <- subset(df, Cluster == i)
    temp$Cluster <- NULL
    sum <- sum(as.vector(apply(temp, 1, FUN=function(p1,p2){
      return(EuclideanDistance(p1,p2)^2)}, centroids[i,])))
    SSE <- append(SSE, sum)
  }
  return(list(clusters=active.clusters, SSE=SSE))
}

# This function will return the similarity matrix.
# It takes a data frame and a list of clusters as well as a titile of the graph
# Data should be normalized
GetSimilarityMatrix <- function(df, clusters, title){
  # Order data by cluster groups
  df["Cluster"] <- NA
  df$Cluster <- clusters
  df <- df[order(df$Cluster),]
  df$Cluster <- NULL
  sim.matrix <- matrix(data=0, ncol=nrow(df), nrow=nrow(df))
  for(i in 1:nrow(df)){
    sim.matrix[i,] <- as.vector(apply(df, 1,EuclideanDistance, df[i,]))
  }
  return(image(sim.matrix, col=topo.colors(10), main=title))
}

# This function returns the SSE value of a data set and its cluster list
# This function is to be used at the end of a run
AllClusterSSE <- function(df, k, clusters){
  natt <- ncol(df)
  df["Cluster"] <- NA
  df$Cluster <- clusters
  df <- df[order(df$Cluster),]
  
  centroids <- as.data.frame(matrix(data=0, nrow=k, ncol=natt))
  centroids <- UpdateCentroids(df, centroids, k)
  #print(df)
  SSE <- 0
  for(i in 1:k){
    temp <- subset(df, Cluster == i)
    temp$Cluster <- NULL
    #print(temp)
    sum <-  sum(as.vector(apply(temp, 1,FUN=function(p1,p2){
      return((EuclideanDistance(p1,p2)^2))}, centroids[i,])))
    SSE <- SSE + sum
  }
  
  return(SSE)
}


#### Algo ####
# Preping data
iris.kmeans <- iris
iris.kmeans$Species <- NULL
iris.kmeans <- NormalizeData(iris.kmeans, ncol(iris.kmeans))

# K equal to 2
k.two.time <- system.time(k.two.result <- MyKMeans(iris.kmeans, 2))
k.two.clusters <- k.two.result$Cluster

# K equal to 3
k.three.result <- MyKMeans(iris.kmeans, 3)
k.three.clusters <- k.three.result$Cluster

# K equal to 4
k.four.result <- MyKMeans(iris.kmeans, 4)
k.four.clusters <- k.four.result$Cluster

## Tables
table(iris$Species, k.two.clusters)
table(iris$Species, k.three.clusters)
table(iris$Species, k.four.clusters)

# Similarity Matrices
GetSimilarityMatrix(iris.kmeans, k.two.clusters, "My K Means: 2 Clusters")
GetSimilarityMatrix(iris.kmeans, k.three.clusters, "My K Means: 3 Clusters")
GetSimilarityMatrix(iris.kmeans, k.four.clusters, "My K Means: 4 Clusters")

# Final SSE Calculation
AllClusterSSE(iris.kmeans, 2, k.two.clusters)
AllClusterSSE(iris.kmeans, 3, k.three.clusters)
AllClusterSSE(iris.kmeans, 4, k.four.clusters)

# Times
print(k.two.time)

# Get SSE Graphs
k.two.result <- MyKMeans(iris.kmeans, 2, TRUE)
k.two.SSE <- k.two.result$SSE
plot(k.two.SSE, c(1:length(k.two.SSE)), xlab="SSE", ylab="Iteration", main="My KMeans, K = 2")

### Notes on what to do next
# Graph SSE for myKMeans.  Probably a list of iteration number and kmeans and create a graph
# from that
# Compare Datasets. Calculate the Entropy and SSE
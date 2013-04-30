# Created by Ricco Amezcua
# First created on 4/22/2013
# Last updated on 4/29/2013
# This file contains the assignment code Assignment 5 for CS 422
# It uses the iris data set

# Function for getting similarity matirx
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
EuclideanDistance <- function(p1, p2){
  distance <- p1-p2
  distance <- sapply(distance, FUN=function(x){return(x^2)})
  distance <- sum(distance)
  distance <- sqrt(distance)
  return (distance)
}
NormalizeData <- function(df, natt){
  for(i in 1:natt){
    stan <- sd(df[,i])
    mean <- mean(df[,i])
    df[,i] <- sapply(df[,i], FUN = function(x){
      return ((x-mean)/stan)}) # nvm (Added a rounding function here round(x, 3))
  }
  return (df)
}


##### K Means Algorithm #####
### Preparing Iris data ###
iris.data <- iris
iris.data$Species <- NULL

# k = Two
two.time <- system.time(kmeans.two<- kmeans(iris.data, 2))

# k = Three
three.time <-system.time(kmeans.three <- kmeans(iris.data, 3))

# k = Four
four.time <- system.time(kmeans.four <- kmeans(iris.data, 4))

#k = Five
five.time <- system.time(kmeans.five <- kmeans(iris.data, 5))

# K = Six
six.time <- system.time(kmeans.six <- kmeans(iris.data, 6))

sink("./data/builtinKmeansdata.txt", split=T)
# Times for K Means
print("Times for built in kmeans")
two.time
three.time
four.time
five.time
six.time


# Matrices
print("Comparison of real classes and clusters")
table(iris$Species, kmeans.two$cluster)
table(iris$Species, kmeans.three$cluster)
table(iris$Species, kmeans.four$cluster)
table(iris$Species, kmeans.five$cluster)
table(iris$Species, kmeans.six$cluster)

# SSE
print("SSE for KMeans")
kmeans.two$tot.withinss
kmeans.three$tot.withinss
kmeans.four$tot.withinss
kmeans.five$tot.withinss
kmeans.six$tot.withinss
sink()

# Similarity matrices
iris.norm <- NormalizeData(iris.data, ncol(iris.data))
GetSimilarityMatrix(iris.norm, kmeans.two$cluster, "K Means, K = 2")
GetSimilarityMatrix(iris.norm, kmeans.three$cluster, "K Means, K = 3")
GetSimilarityMatrix(iris.norm, kmeans.four$cluster, "K Means, K = 4")
GetSimilarityMatrix(iris.norm, kmeans.five$cluster, "K Means, K = 5")
GetSimilarityMatrix(iris.norm, kmeans.six$cluster, "K Means, K = 6")


#Plots
plot(iris.data, col=kmeans.two$cluster, main="K Means, K = 2")
plot(iris.data, col=kmeans.three$cluster, main="K Means, K = 3")
plot(iris.data, col=kmeans.four$cluster, main="K Means, K = 4")
plot(iris.data, col=kmeans.five$cluster, main="K Means, K = 5")
plot(iris.data, col=kmeans.six$cluster, main="K Means, K = 6")



##### Hierachical Clustering ####
# Sampling the iris data
i <- sample(1:dim(iris)[1], 30) # Getting 30 samples
iris.sample <- iris[i,]

sink("./data/hierachClustering.txt", split=T)
# Clustering
# Using the average method
hc <- hclust(dist(iris.sample), method="ave")
plot(hc, hang= -1,labels=iris$Species[i], main="Cluster: Average Method")

# Using the centroid method
hc <- hclust(dist(iris.sample), method="cen")
plot(hc, hang= -1,labels=iris$Species[i], main="Cluster: Centroid Method")

# Using the median method
hc <- hclust(dist(iris.sample), method="median")
plot(hc, hang= -1,labels=iris$Species[i], main="Cluster: Median Method")

# Using the single method
hc <- hclust(dist(iris.sample), method="single")
plot(hc, hang= -1,labels=iris$Species[i], main="Cluster: Single Method")

rm(i)
sink()

#### DB Scan ####
library("fpc")

iris.data <- iris[-5]

sink("./data/dbscanData.txt", split=T)
# Eps 0.4
db1 <- dbscan(iris.data, eps=0.4, MinPts=5)

# Eps 0.5
db2 <- dbscan(iris.data, eps=0.5, MinPts=5)

# Eps 0.6
db3 <- dbscan(iris.data, eps=0.6, MinPts=5)

sink("./data/dbscanData.txt", split=T)
# Comparison Matrices
table(iris$Species, db1$cluster)
table(iris$Species, db2$cluster)
table(iris$Species, db3$cluster)
sink()

# Plots
plotcluster(iris.data,db1$cluster, main="DB Scan: Eps = 0.4")
plotcluster(iris.data,db2$cluster, main="DB Scan: Eps = 0.5")
plotcluster(iris.data,db3$cluster, main="DB Scan: Eps = 0.6")

#Similarity Matrices
GetSimilarityMatrix(iris.norm, db1$cluster, "DB Scan: Eps = 0.4")
GetSimilarityMatrix(iris.norm, db2$cluster, "DB Scan: Eps = 0.5")
GetSimilarityMatrix(iris.norm, db3$cluster, "DB Scan: Eps = 0.6")




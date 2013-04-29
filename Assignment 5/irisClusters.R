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

# Similarity matrices
GetSimilarityMatrix(iris.data, kmeans.two$cluster, "K Means, K = 2")
GetSimilarityMatrix(iris.data, kmeans.three$cluster, "K Means, K = 3")
GetSimilarityMatrix(iris.data, kmeans.four$cluster, "K Means, K = 4")
GetSimilarityMatrix(iris.data, kmeans.five$cluster, "K Means, K = 5")
GetSimilarityMatrix(iris.data, kmeans.six$cluster, "K Means, K = 6")


#Plots
plot(iris.data, col=kmeans.two$cluster, main="K Means, K = 2")
plot(iris.data, col=kmeans.three$cluster, main="K Means, K = 3")
plot(iris.data, col=kmeans.four$cluster, main="K Means, K = 4")
plot(iris.data, col=kmeans.five$cluster, main="K Means, K = 5")
plot(iris.data, col=kmeans.six$cluster, main="K Means, K = 6")



##### Hierachical Clustering ####
# Sampling the iris data
i <- sample(1:dim(iris)[1], 40) # Getting 30 samples
iris.sample <- iris[i,]

# Clustering
# Using the average method
hc <- hclust(dist(iris.sample), method="ave")
plot(hc, hang= -1,labels=iris$Species[i])

# Using the centroid method
hc <- hclust(dist(iris.sample), method="cen")
plot(hc, hang= -1,labels=iris$Species[i])

# Using the median method
hc <- hclust(dist(iris.sample), method="median")
plot(hc, hang= -1,labels=iris$Species[i])

# Using the single method
hc <- hclust(dist(iris.sample), method="single")
plot(hc, hang= -1,labels=iris$Species[i])

rm(i)

#### DB Scan ####
library("fpc")

iris.data <- iris[-5]

# Eps 0.4
db <- dbscan(iris.data, eps=0.4, MinPts=5)
table(iris$Species, db$cluster)
plotcluster(iris.data,db$cluster)

# Eps 0.5
db <- dbscan(iris.data, eps=0.5, MinPts=5)
table(iris$Species, db$cluster)
plotcluster(iris.data,db$cluster)

# Eps 0.6
db <- dbscan(iris.data, eps=0.6, MinPts=5)
table(iris$Species, db$cluster)
plotcluster(iris.data,db$cluster)




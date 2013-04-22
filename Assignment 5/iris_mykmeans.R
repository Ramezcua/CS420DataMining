#### Functions ####
NormalizeData <- function(df, natt){
  for(i in 1:natt){
    stan <- sd(df[,i])
    mean <- mean(df[,i])
    df[,i] <- sapply(df[,i], FUN = function(x){
      return ((x-mean)/stan)}) # nvm (Added a rounding function here round(x, 3))
  }
  return (df)
}

UpdateCentroids <- function(df, centroids, k){
  for(i in 1:k){
    temp <- subset(df, Cluster == i)
    temp$Cluster <- NULL # Remove clusters
    centroids[i,]<-as.vector(colMeans(temp))
  }
  
  return (centroids)
}

EuclideanDistance <- function(p1, p2){
  distance <- p1-p2
  distance <- sapply(distance, FUN=function(x){return(x^2)})
  distance <- sum(distance)
  distance <- sqrt(distance)
  return (distance)
}

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

#### Algo ####

# Creating data
k.three <- iris
k.three$Species <- NULL
k <- 3
natt <- ncol(k.three)
centroids <- as.data.frame(matrix(data=0, nrow=k, ncol=natt))

# Normalizing the data set
k.three <- NormalizeData(k.three, natt)

# Adding a cluster column and randomly assigning each point to a cluster
k.three["Cluster"] <- NA
k.three$Cluster <- sample(1:k, nrow(k.three), replace=TRUE)

# Initiale update of points
centroids <- UpdateCentroids(k.three, centroids, k)


stopping <- TRUE
i <- 0
while(stopping){
  temp <- k.three
  # remove cluster column for easier calculation
  temp$Cluster <- NULL
  centroids <- UpdateCentroids(k.three, centroids, k)
  k.three$Cluster <- apply(temp, 1,FindCluster, centroids)
  
  i <- i + 1
  if (i > 10){
    stopping <- FALSE
  }
}


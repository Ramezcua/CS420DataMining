
##### K Means Algorithm #####
### Preparing Iris data ###
iris.data <- iris
iris.data$Species <- NULL
### k = Two- Unsupervised ###
kmeans.two<- kmeans(iris.data, 2)
kmeans.two

table(iris$Species, kmeans.two$cluster)
plot(k.two[c("Sepal.Length", "Sepal.Width")], col = kmeans.two$cluster)+ 
  points(kmeans.two$centers[,c("Sepal.Length", "Sepal.Width")], 
         col = 1:3, pch = 8, cex=2)

### k Three
kmeans.three <- kmeans(iris.data, 3)
kmeans.three

table(iris$Species, kmeans.three$cluster)
plot(k.three[c("Sepal.Length", "Sepal.Width")], col = kmeans.three$cluster)+ 
  points(kmeans.three$centers[,c("Sepal.Length", "Sepal.Width")], 
  col = 1:3, pch = 8, cex=2)

### k Four
kmeans.four <- kmeans(iris.data, 4)
kmeans.four

table(iris$Species, kmeans.four$cluster)
plot(k.four[c("Sepal.Length", "Sepal.Width")], col = kmeans.four$cluster)+ 
  points(kmeans.four$centers[,c("Sepal.Length", "Sepal.Width")], 
         col = 1:3, pch = 8, cex=2)



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




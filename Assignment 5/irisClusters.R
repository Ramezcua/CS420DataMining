
#### K Means Algorithm
### k = Two- Unsupervised
k.two <- iris
k.two$Species <- NULL
kmeans.two<- kmeans(k.two, 2)
kmeans.two

table(iris$Species, kmeans.two$cluster)
plot(k.two[c("Sepal.Length", "Sepal.Width")], col = kmeans.two$cluster)+ 
  points(kmeans.two$centers[,c("Sepal.Length", "Sepal.Width")], 
         col = 1:3, pch = 8, cex=2)

### k = Three - Supervised
k.three <- iris
k.three$Species <- NULL
kmeans.three <- kmeans(k.three, 3)
kmeans.three

table(iris$Species, kmeans.iris.three$cluster)
plot(iris.three[c("Sepal.Length", "Sepal.Width")], col = kmeans.iris.three$cluster)+ 
  points(kmeans.iris.three$centers[,c("Sepal.Length", "Sepal.Width")], 
  col = 1:3, pch = 8, cex=2)

### k = Four - Supervised
iris.four <- iris
iris.four$Species <- NULL
kmeans.iris.four <- kmeans(iris.four, 4)
kmeans.iris.four

table(iris$Species, kmeans.iris.four$cluster)
plot(iris.four[c("Sepal.Length", "Sepal.Width")], col = kmeans.iris.four$cluster)+ 
  points(kmeans.iris.four$centers[,c("Sepal.Length", "Sepal.Width")], 
         col = 1:3, pch = 8, cex=2)

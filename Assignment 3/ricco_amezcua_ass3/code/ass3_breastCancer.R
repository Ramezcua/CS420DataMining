# Created by Ricco Amezcua
# This lab uses the breast cancer data set from the UCI repository
# For the class column: 2 is for benign, 4 for malignant
# This is later changed to 0 for benign and 1 for malignant

### Accuracy Function ####
Accuracy <- function(conf.mat){
  a <- conf.mat[1,1]
  b <- conf.mat[1,2]
  c <- conf.mat[2,1]
  d <- conf.mat[2,2]
  
  return ((a+d)/(a+b+c+d))
}

#### Reading in data ####
cancer.data <- read.csv("./breast_cancer/breast-cancer-wisconsin.data", header=F)

### Cleaning of data ###

# Naming the columns
names(cancer.data)<-c("ID",
                    "clump.thickness",
                    "cell.size",
                    "cell.shape",
                    "adhesion",
                    "epithelial.cell.size",
                    "bare.nuclei",
                    "bland.chromatin",
                    "normal.nucleoli",
                    "mitoses",
                    "class")

# Get rid of ID column
cancer.data <- cancer.data[,-1]

# Change bare.nuclei from factor to numeric
cancer.data$bare.nuclei <- as.numeric(cancer.data$bare.nuclei)

# Change class for classes
cancer.data$class <- ifelse(cancer.data$class == 2, 0, 1)

# Change class from numeric to factor
cancer.data$class <- as.factor(cancer.data$class)

cancer.formula <- class ~ clump.thickness +
              cell.size +
              cell.shape +
              adhesion +
              epithelial.cell.size +
              bare.nuclei +
              bland.chromatin +
              normal.nucleoli +
              mitoses

# Getting the samples of 80% for traning and 20% for testing
i <- sample(2, nrow(cancer.data), replace = T, prob = c(0.8, 0.2))
train.set <- cancer.data[i==1,]
test.set <- cancer.data[i==2,]
rm(i)


### Naive Bayes Classifier ###
library(e1071)
naive.bayes.cancer <-naiveBayes(cancer.formula, train.set)
sink("./data/cancer_naive_bayes_classifier.txt", split=T)
nbcancer.conf.mat <- table(test.set$class, predict(naive.bayes.cancer, test.set))
nbcancer.accuracy <- Accuracy(nbcancer.conf.mat)
nbcancer.error <- 1 - nbcancer.accuracy
nbcancer.conf.mat
nbcancer.accuracy
nbcancer.error
sink()

### SVM Classifier ###
svm.cancer <-svm(cancer.formula, train.set)
sink("./data/cancer_svm_classifier.txt", split=T)
svmcancer.conf.mat <- table(test.set$class, predict(svm.cancer, test.set))
svmcancer.accuracy <- Accuracy(svmcancer.conf.mat)
svmcancer.error <- 1 - svmcancer.accuracy
svmcancer.conf.mat
svmcancer.accuracy
svmcancer.error
sink()
detach(e1071)


### Neural Network Classifier ###
library(nnet)
nn.cancer <- nnet(cancer.formula, train.set, size = 4)
sink("./data/cancer_nn_classifier.txt", split=T)
nncancer.conf.mat <- table(test.set$class, round(predict(nn.cancer, test.set)))
nncancer.accuracy <- Accuracy(nncancer.conf.mat)
nncancer.error <- 1 - nncancer.accuracy
nncancer.conf.mat
nncancer.accuracy
nncancer.error
sink()
detach(nnet)

### Logistic Regression Classifier ###
log.rep.cancer <- glm(cancer.formula, family=binomial(logit), data=train.set)
sink("./data/cancer_log_classifier.txt", split=T)
lrcancer.conf.mat <- table(test.set$class, predict(log.rep.cancer, test.set) > 0)
lrcancer.accuracy <- Accuracy(lrcancer.conf.mat)
lrcancer.error <- 1 - lrcancer.accuracy
lrcancer.conf.mat
lrcancer.accuracy
lrcancer.error
sink()


##############################################################################
### Perceptron ###

# This function takes a dataframe and returns a vector with the mean
# of all of the attributes
MeanVector <- function(df){
  means <- NULL
  
  for(i in 1:ncol(df)){
    means <- c(means, mean(df[,i]))
  }
  
  return (means)
}

# This function takes a dataframe and returns a vector with the standard deviation
# of all of the attributes
StanDeviationVector <- function(df){
  stans <- NULL
  
  for(i in 1:ncol(df)){
    stans <- c(stans, sd(df[,i]))
  }
  
  return (stans)
}

# This function will take a dataframe and return a dataframe
# the values "normalized".  They are normalized by taking a value
# and subtracting it by the mean and divinding by the standard deviation
# of the attribute.  Currently, this is a large bottleneck for the system.
NormalizedDF <- function(df){
  means <- MeanVector(df)
  stand.d <- StanDeviationVector(df)
  for(i in 1:ncol(df)){
    df[,i] <- sapply(df[,i], FUN = function(x){
      return ((x-means[i])/stand.d[i])})
  }
  
  return(df)
}


PerceptronSign<- function(x, w, threshold){
  result <- x %*% w
  #switching from 0 to -1
  if(result[1,1] > threshold){ return (1)}
  else { return (-1)}
}




Perceptron <- function(df, lr, runs){
  
  learn.rate <- lr
  threshold <- 0
  runs <- runs
  w <- rep(0, ncol(df)) # Create an empty weight vector
  
  #Change 0's to -1
  df$class <- ifelse(df$class == 0, -1, 1)
  
  # Give all columns except the one with the class
  normalized.df <- NormalizedDF(df[,-10])
  
  #x <- c(1, x)
  for(i in 1:runs){
    for( i in 1:nrow(df)){
      # Get x vector from data frame
      x <- c(1, as.numeric(as.vector(normalized.df[i,])))
      z <- as.numeric(df[i, 10])
      n <- PerceptronSign(x, w, threshold) 
      if (z != n){
      # There is a misclassification
        error <- z-n
        correction <- learn.rate * error
         w <- w + (correction * x)
      }
    }
  }
  return (w)
}

PerceptronClassify <- function(df, w){
  threshold <- 0
  results <- NULL
  
  for(i in 1:nrow(df)){
    x <- c(1, as.numeric(as.vector(df[i, -10])))
    if (PerceptronSign(x, w, threshold) == 1){
      results <- c(results, 1)
    }
    else{
      results <- c(results,0)
    }
  }
  return (results)
}

# Have 25 runs
weights25 <- Perceptron(train.set, 0.1, 25)
sink("./data/cancer_my_percep25_classifier.txt", split=T)
myp25cancer.conf.mat <-table(test.set$class,PerceptronClassify(test.set, weights25))
myp25cancer.accuracy <- 88/(88+45)
myp25cancer.error <- 1 - myp25cancer.accuracy
myp25cancer.conf.mat
myp25cancer.accuracy
myp25cancer.error
sink()

weights10 <- Perceptron(train.set, 0.1, 10)
sink("./data/cancer_my_percep10_classifier.txt", split=T)
myp10cancer.conf.mat <-table(test.set$class,PerceptronClassify(test.set, weights10))
myp10cancer.accuracy <- 88/(88+45)
myp10cancer.error <- 1 - myp10cancer.accuracy
myp10cancer.conf.mat
myp10cancer.accuracy
myp10cancer.error
sink()

rm(list=ls())
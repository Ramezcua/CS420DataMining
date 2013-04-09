# Created by Ricco Amezcua

### Accuracy Function ####
Accuracy <- function(conf.mat){
  a <- conf.mat[1,1]
  b <- conf.mat[1,2]
  c <- conf.mat[2,1]
  d <- conf.mat[2,2]
  
  return ((a+d)/(a+b+c+d))
}


### Reading in the file ###
spam.base <- read.csv("./spambase/spambase.data", header=F, 
                     colClasses=c(rep('numeric',57), 'factor'))

# Naming the columns
names(spam.base)<-c("word_freq_make",
                   "word_freq_address",
                   "word_freq_all",
                   "word_freq_3d",
                   "word_freq_our",
                   "word_freq_over",
                   "word_freq_remove",
                   "word_freq_internet",
                   "word_freq_order",
                   "word_freq_mail",
                   "word_freq_receive",
                   "word_freq_will",
                   "word_freq_people",
                   "word_freq_report",
                   "word_freq_addresses",
                   "word_freq_free",
                   "word_freq_business",
                   "word_freq_email",
                   "word_freq_you",
                   "word_freq_credit",
                   "word_freq_your",
                   "word_freq_font",
                   "word_freq_000",
                   "word_freq_money",
                   "word_freq_hp",
                   "word_freq_hpl",
                   "word_freq_george",
                   "word_freq_650",
                   "word_freq_lab",
                   "word_freq_labs",
                   "word_freq_telnet",
                   "word_freq_857",
                   "word_freq_data",
                   "word_freq_415",
                   "word_freq_85",
                   "word_freq_technology",
                   "word_freq_1999",
                   "word_freq_parts",
                   "word_freq_pm",
                   "word_freq_direct",
                   "word_freq_cs",
                   "word_freq_meeting",
                   "word_freq_original",
                   "word_freq_project",
                   "word_freq_re",
                   "word_freq_edu",
                   "word_freq_table",
                   "word_freq_conference",
                   "char_freq_sem_col",
                   "char_freq_c_par",
                   "char_freq_s_par",
                   "char_freq_exc_mrk",
                   "char_freq_dollar_sgn",
                   "char_freq_hash_sgn",
                   "capital_run_length_average",
                   "capital_run_length_longest",
                   "capital_run_length_total",  
                   "spam")

spam.formula <- spam ~ word_freq_make+
  word_freq_address+
  word_freq_all+
  word_freq_3d+
  word_freq_our+
  word_freq_over+
  word_freq_remove+
  word_freq_internet+
  word_freq_order+
  word_freq_mail+
  word_freq_receive+
  word_freq_will+
  word_freq_people+
  word_freq_report+
  word_freq_addresses+
  word_freq_free+
  word_freq_business+
  word_freq_email+
  word_freq_you+
  word_freq_credit+
  word_freq_your+
  word_freq_font+
  word_freq_000+
  word_freq_money+
  word_freq_hp+
  word_freq_hpl+
  word_freq_george+
  word_freq_650+
  word_freq_lab+
  word_freq_labs+
  word_freq_telnet+
  word_freq_857+
  word_freq_data+
  word_freq_415+
  word_freq_85+
  word_freq_technology+
  word_freq_1999+
  word_freq_parts+
  word_freq_pm+
  word_freq_direct+
  word_freq_cs+
  word_freq_meeting+
  word_freq_original+
  word_freq_project+
  word_freq_re+
  word_freq_edu+
  word_freq_table+
  word_freq_conference+
  char_freq_sem_col+
  char_freq_c_par+
  char_freq_s_par+
  char_freq_exc_mrk+
  char_freq_dollar_sgn+
  char_freq_hash_sgn+
  capital_run_length_average+
  capital_run_length_longest+
  capital_run_length_total 

# Getting the samples of 80% for traning and 20% for testing
i <- sample(2, nrow(spam.base), replace = T, prob = c(0.8, 0.2))
train.set <- spam.base[i==1,]
test.set <- spam.base[i==2,]
rm(i)

### Naive Bayes Classifier ###
library(e1071)
naive.bayes.spam <-naiveBayes(spam.formula, train.set)
sink("./data/spam_naive_bayes_classifier.txt", split=T)
nbspam.conf.mat <- table(test.set$spam, predict(naive.bayes.spam, test.set))
nbspam.accuracy <- Accuracy(nbspam.conf.mat)
nbspam.error <- 1 - nbspam.accuracy
nbspam.conf.mat
nbspam.accuracy
nbspam.error
sink()

### SVM Classifier ###
svm.spam <-svm(spam.formula, train.set)
sink("./data/spam_svm_classifier.txt", split=T)
svmspam.conf.mat <- table(test.set$spam, predict(svm.spam, test.set))
svmspam.accuracy <- Accuracy(svmspam.conf.mat)
svmspam.error <- 1 - svmspam.accuracy
svmspam.conf.mat
svmspam.accuracy
svmspam.error
sink()


### Neural Network Classifier ###
library(nnet)
nn.spam <- nnet(spam.formula, train.set, size = 4)
sink("./data/spam_nn_classifier.txt", split=T)
nnspam.conf.mat <- table(test.set$spam, round(predict(nn.spam, test.set)))
nnspam.accuracy <- Accuracy(nnspam.conf.mat)
nnspam.error <- 1 - nnspam.accuracy
nnspam.conf.mat
nnspam.accuracy
nnspam.error
sink()


### Logistic Regression Classifier ###
log.rep.spam <- glm(spam.formula, family=binomial(logit), data=train.set)
sink("./data/spam_log_classifier.txt", split=T)
lrspam.conf.mat <- table(test.set$spam, predict(log.rep.spam, test.set) > 0)
lrspam.accuracy <- Accuracy(lrspam.conf.mat)
lrspam.error <- 1 - lrspam.accuracy
lrspam.conf.mat
lrspam.accuracy
lrspam.error
sink()


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
  df$spam <- ifelse(df$spam == 0, -1, 1)
  
  # Give all columns except the one with the class
  normalized.df <- NormalizedDF(df[,-58])
  
  #x <- c(1, x)
  for(i in 1:runs){
    for( i in 1:nrow(df)){
      # Get x vector from data frame
      x <- c(1, as.numeric(as.vector(normalized.df[i,])))
      z <- as.numeric(df[i, 58])
      n <- PerceptronSign(x, w, threshold) 
      #if (z != n){
        # There is a misclassification
      error <- z-n
      correction <- learn.rate * error
      w <- w + (correction * x)
      #}
    }
  }
  return (w)
}

PerceptronClassify <- function(df, w){
  threshold <- 0
  results <- NULL
  
  for(i in 1:nrow(df)){
    x <- c(1, as.numeric(as.vector(df[i, -58])))
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
sink("./data/spam_my_percep25_classifier.txt", split=T)
myp25spam.conf.mat <-table(test.set$spam,PerceptronClassify(test.set, weights25))
myp25spam.accuracy <- Accuracy(myp25spam.conf.mat)
myp25spam.error <- 1 - myp25spam.accuracy
myp25spam.conf.mat
myp25spam.accuracy
myp25spam.error
sink()

# Have 10 runs
weights10 <- Perceptron(train.set, 0.1, 10)
sink("./data/spam_my_percep10_classifier.txt", split=T)
myp10spam.conf.mat <-table(test.set$spam,PerceptronClassify(test.set, weights10))
myp10spam.accuracy <- Accuracy(myp10spam.conf.mat)
myp10spam.error <- 1 - myp10spam.accuracy
myp10spam.conf.mat
myp10spam.accuracy
myp10spam.error
sink()

rm(list=ls())
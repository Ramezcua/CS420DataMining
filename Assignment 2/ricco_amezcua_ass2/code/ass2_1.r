# Created by Ricco Amezcua

spambase <- read.csv("./spambase/spambase.data", header=F, colClasses=c(rep('numeric',57), 'factor'))


# Naming the columns
names(spambase)<-c("word_freq_make",
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

# Converting 0 and 1 in spam column to not_spam and spam (better labels)
spambase$spam <- ifelse(spambase$spam == 0, c("not_spam"), c("spam"))

                
# Getting the samples of 80% for traning and 20% for testing
i <- sample(2, nrow(spambase), replace = T, prob = c(0.8, 0.2))
trainSet <- spambase[i==1,]
testSet <- spambase[i==2,]

library('rpart')

spamFormula <- spam ~ word_freq_make+
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

spam_rpart <- rpart(spamFormula, data = trainSet)

#Tree Pruning 
spam_rpartPrune <- prune(spam_rpart, cp=0.017) #0.017 removed three branches 

#print(spam_rpart)
library(rpart.plot)
#prp(spam_rpart)
#Spam Decision Tree
prp(spam_rpart, type =2, extra = 4, main = "Spam Decision Tree")
plotcp(spam_rpart)

#Spam Decision Tree - Pruned
prp(spam_rpartPrune, type =2, extra = 4, main = "Spam Pruned Decision Tree")

#printcp(spam_rpart)

# Confusion Matrix for prediction on Training Set
predTrain <- predict(spam_rpart, newdata=trainSet, type="class")
confMatTrain <- table(trainSet$spam, predTrain)
sink("./data/prediction_trainingSet.txt", split=T)
print(confMatTrain)
sink()

# Confusion Matrix for prediction on Testing Set
predTest <- predict(spam_rpart, newdata=testSet, type="class")
confMatTest <- table(testSet$spam, predTest)
sink("./data/prediction_testingSet.txt", split=T)
print(confMatTest)
sink()

# Confusion Matrix for prediction on Training Set using a pruned tree 
predTrainPrune <- predict(spam_rpartPrune, newdata=trainSet, type="class")
confMatTrainPrune <- table(trainSet$spam, predTrainPrune)
sink("./data/prediction_trainingSet_pruned.txt", split=T)
print(confMatTrainPrune)
sink()

# Confusion Matrix for prediction on Testing Set using a pruned tree
predTestPrune <- predict(spam_rpartPrune, newdata=testSet, type="class")
confMatTestPrune <- table(testSet$spam, predTestPrune)
sink("./data/prediction_testingSet_pruned.txt", split=T)
print(confMatTestPrune)
sink()


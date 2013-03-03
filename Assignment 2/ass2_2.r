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


best_split <- function(df){
  # 58 is a magic number because it refers to the last row in df, which is spam in spambase
  # Didn't have time to figure out how formula works
  att_num <- 1
  sorted_values <- sort(df[,att_num])
  temp_continous_frame <- data.frame(value=numeric(0), not_spam_Above=numeric(0), not_spam_Below=numeric(0), spam_Above=numeric(0), spam_Below=numeric(0))
	best_pick <-NULL
  checked_values <- NULL
  split_points <- makeSplitPoints(unique(sorted_values))
  sorted_df <- df[order(df[,att_num]),]
  first_above_not_spam <- 0
  first_above_spam <- 0
  for (i in 1:nrow(sorted_df)){
    if (sorted_df[i,58] == 'not_spam'){ first_above_not_spam <- first_above_not_spam + 1}
    else {first_above_spam <- first_above_spam + 1}
  }
  
  temp_continous_frame <- c(split_points[1], first_above_not_spam, 0, first_above_spam, 0)
  
  for (j in 2:length(split_points)){
    while(split_points[j] > sorted_df[1,att_num]){
      #above_not_spam <- 0 Maybe add these........
      #above_spam <- 0
    }
  }
  
  
  
  #for (i in 1:length(sorted_values))
    #if (sorted_values[i]%in%checked_values == FALSE){
      # Add this new value to check values
     # checked_values <- c(checked_values, sorted_values[i])
      # Variables to be filled
    #  not_spam_Above <- 0
  #    not_spam_Below <- 0
 #     spam_Above <- 0
#      #spam_Below <- 0
      
      # Now have to counted the spams and not_spams above and below i
      # Change 1 in df[j,1] later
      #for (j in 1:length(sorted_values)){
     #   if (df[j,58] == 'not_spam' && sorted_values[i] >= df[j,1]){
    #      not_spam_Above <- not_spam_Above + 1
   #     }else if (df[j,58] == 'not_spam' && sorted_values[i] < df[j,1]){
  #        not_spam_Below <- not_spam_Below + 1
 #       }else if (df[i,58] == 'spam' && sorted_values[i] >= df[j,1]){
#          spam_Above <- spam_Above + 1
#        }else if (df[i,58] == 'spam' && sorted_values[i] < df[j,1]){
          #spam_Below <- spam_Below + 1
        #}
      #}
    
    #temp_continous_frame[i,](1:100) <- c(i, not_spam_Above, not_spam_Below, spam_Above, spam_Below)
    #}
    #print(temp_continous_frame)
}

myBinaryGini <- function(x,y){
  return( 1 - ((x/(x+y))^2) - ((y/(x+y))^2) )
}

makeSplitPoints <- function(points){
  # Adding the first point from points as it will be the first point in split points
  splitpoints <- points[1]
  while (length(points) > 1){
    splitpoints <- c(splitpoints, (points[1]+points[2])/2)
    points <- points[-1]
  }
  
  return (splitpoints)
}

best_split(spambase)
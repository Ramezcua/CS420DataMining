
MakeSplitPoints <- function(points){
  # Adding the first point from points as it will be the first point in split points
  splitpoints <- points[1]
  while (length(points) > 1){
    splitpoints <- c(splitpoints, (points[1]+points[2])/2)
    points <- points[-1]
  }
  
  return (splitpoints)
}

spambase <- read.csv("./spambase/spambase.data", header=F, 
                     colClasses=c(rep('numeric',57), 'factor'))


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
train.set <- spambase[i==1,]
test.set <- spambase[i==2,]


BestSplit <- function(df){
  # 58 is a magic number because it refers to the last row in df, which is spam in spambase
  # Didn't have time to figure out how formula works
  att.num <- 1
  sorted.values <- sort(df[,att.num])
  temp.continous.frame <- data.frame(value=numeric(0), 
                          not.spam.above=numeric(0), 
                          not.spam.below=numeric(0), 
                          spam.above=numeric(0), 
                          spam.below=numeric(0))  
	best.pick <-NULL
  checked.values <- NULL
  split.points <- MakeSplitPoints(unique(sorted.values))
  sorted.df <- df[order(df[,att.num]),] # Sort the data frame by the selected attribute
  
  # find the spam and not spam for the first 
  first.above.not.spam <- 0
  first.above.spam <- 0
  for (i in 1:nrow(sorted.df)){
    if (sorted.df[i,58] == 'not_spam'){ first.above.not.spam <- first.above.not.spam + 1}
    else {first.above.spam <- first.above.spam + 1}
  }
  
  # Add them to the temp data frame that will hold all values
  temp.continous.frame <- rbind(temp.continous.frame, data.frame(
                          value = split.points[1], 
                          not.spam.above = first.above.not.spam, 
                          not.spam.below = 0, 
                          spam.above = first.above.spam, 
                          spam.below = 0))
  
  # Now go through the other split points
  for (j in 2:length(split.points)){
    # Values for the last entry in temp frame (used for split point checking)
    k <- j - 1
    #print(j)
    ns.above <- temp.continous.frame[k, "not.spam.above"] 
    ns.below <- temp.continous.frame[k, "not.spam.below"] 
    s.above <- temp.continous.frame[k, "spam.above"] 
    s.below <- temp.continous.frame[k, "spam.below"] 
    
    # Check if the first entry 
    while(split.points[j] > sorted.df[1,att.num]){
      if (sorted.df[1, 58] == 'notspam') {
        # a point must be moved from not spam above to not spam below
        ns.below <- ns.bleow + 1
        ns.above <- ns.above - 1
      } else if (sorted.df[1,58] == 'spam'){
        # a point must be moved from spam above to spam below
        s.below <- s.below + 1
        s.above <- s.above - 1
      }
      # Now the entry must be erased from sorted.df so it doesn't get counted again
      sorted.df <- sorted.df[-1,]
    }
    
    # All points under split point j have been counted and so j must be added to the 
    # temp data frame
    temp.continous.frame <- rbind(temp.continous.frame, data.frame(
      value = split.points[j], 
      not.spam.above = ns.above, 
      not.spam.below = ns.below, 
      spam.above = s.above, 
      spam.below = s.below))
    
  }
  
  print(temp.continous.frame)
}

BestSplit(test.set)
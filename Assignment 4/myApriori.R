library(arules)
data(Groceries)
# Convert from transactions to data.frame
test.set <- as(Groceries, "matrix")
# Get a subset of the first 20 entries for testing
test.set <-test.set[1:20,]
# Set columns to alphabetical order
test.set <- test.set[,order(colnames(test.set))]
# Get a list of the names of the groceries
names <- colnames(test.set) 

# Table used for indexing grocery items
names.table <- as.data.frame(names)
# Used to find the index for a grocery item
# This is done for beef
as.numeric(which(names.table[,1] == "beef"))


#### Need to make a list of lists
# Example of appending to inner list
i[[169]] <- append(i[[169]], list("butter"))
as.character() to parse to "string"
# Making a list of lists from the names of groceries
k1 <- lapply(names,list)
# To get to the inner list
k1[[10]]
#To get specific items
k1[[1]][1]

GenerateCandidates <- function(itemset){
  a.items <- itemset
  b.items <- itemset 
}

SupportCounting <- function(itemset){
  buckets <- data.frame(items=I(itemset), support=rep(itemset))
}


Support1ItemCounting <- function(itemset){
  buckets <- data.frame(items=I(itemset), support=rep(itemset))
  c1 <- c(2)
  c2 <- c(3)
  c3 <- c(4)
  
  # Look up table for leaves
  hashbranches <- data.frame(c1=c1, c2=c2, c3=c3)
  
  # Table that points to buckets in buckets frame
  hashleaves <- cbind(c(2,3,4),matrix(0, nrow = 3, ncol=100))
  
  # Fill in hash leaves
  for(i in 1:nrow(buckets)){
    # Get the items of the bucket
    bucket <- buckets[i,1]
    # get the value of the has function (mod 3)
    hash.val <- as.numeric(which(names.table[,1] == bucket[[1]])) %% 3
    # Get the index of the leaf from the branches
    leaf.index <- hashbranches[1,(hash.val+1)]
    search.buckets <- TRUE
    j <- 2
    while(search.buckets){
      if (hashleaves[(leaf.index-1),j] == 0){
        # Change to pointer of bucket
        hashleaves[(leaf.index-1),j] <- i
        search.buckets <- FALSE 
      }
      j <- j + 1
    }
  }
  
  # Do support counting
  for(i in 1:nrow(test.set)){
    # Get first item from list and pass it through has function (mod 3)
    hash.val <- as.numeric(as.vector(which(test.set[i,] == 1))[1]) %% 3
    
    # Get the index of the leaf from the branches (have to add one because it starts at 1)
    leaf.index <- hashbranches[1,(hash.val+1)]
    
    # Get  a vector that includes a list of plausible buckets
    bucket.list <- as.data.frame(hashleaves)[-1,leaf.index]
    # Remove the first item since it is the leaf index
    #bucket.list <- bucket.list[-1]
    #bucket.list <- bucket.list[bucket.list!=0]
    
    for(j in 1:length(bucket.list)){
      transaction <- as.vector(which(test.set[1,] == 1))
      #Candidate is returning a null value
      candidate <- as.vector(lapply(x[bucket.list[j],1], namelookup))
      # If candidate is in transactions (they have no difference)
      print(candidate)
      if(diff(candidate %in% transaction) == 0){
        # Increment the support by 1
        buckets[bucket.list[j], 2] <- buckets[bucket.list[j], 2] + 1       
      }
    }
    
  }

  return(buckets); 
}

namelookup <- function(name){
  return(as.numeric(which(names.table[,1]==as.character(name))))
}








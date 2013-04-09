### Created by Ricco Amezcua
### This file containts the script for part 1 of assignment 4.
### This script uses arules and arulesViz library to create apriori rules
### Different paraters are tested for the rules.  
### Rules are viszulized 
### The output of code can be found in the data folder

library(arules)
library(arulesViz)
data(Groceries)
#i <- as(Groceries, "matrix")
#rules <- apriori(Groceries)

rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.01, target = "rules"))
rules
plot(rules)
inspect(head(sort(rules, by="support"), 10))
inspect(head(sort(rules, by="lift"), 10))
inspect(head(sort(rules, by="confidence"), 10))

rules <- apriori(Groceries, parameter = list(supp = 0.005, conf = 0.005, target = "rules"))




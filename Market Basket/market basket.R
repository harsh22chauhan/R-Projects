#Market Basket Analysis...
rm(list=ls())
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("datasets")
data("Groceries")
itemFrequencyPlot(Groceries,topN=20,type="absolute")


#get the rules

rules <- apriori(Groceries,parameter = list(supp = 0.001, conf = 0.8))

#show the top 5 rules, but only 2 digits          
summary(rules)


options(digits = 2)
inspect(rules[1:5])

options(digits = 2)
 inspect(rules[10:20])
rules
print(rules)
 rules = sort(rules,by = "confidence",decreasing = T)
inspect(rules[1:5])

subset.matrix = is.subset(rules,rules)
subset.matrix[lower.tri(subset.matrix,diag = T)] = NA
redundant = colSums(subset.matrix,na.rm = T) >= 1
rules.pruned = rules[!redundant]
rules = rules.pruned
inspect(rules)


rules <- apriori(Groceries,parameter = list(supp = 0.001, conf = 0.8,minlen = 2),appearance = list(default = "lhs",rhs= "whole milk"),control = list(verbose = F))
rules = sort(rules, decreasing = T , by="confidence")
inspect(rules)

options(digits = 2)
inspect(rules[1:5])

top10ssubRules <- head(rules,n=10,by = "confidence")
plot(top10ssubRules,method = "graph",interactive= F,shading = NA)
plot(top10ssubRules,method = "graph",engine = "htmlwidget")

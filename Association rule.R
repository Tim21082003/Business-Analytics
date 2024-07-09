#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)



myData <- read.transactions("market_basket_retails.csv", format = "basket", sep = ",")
inspect(myData[1:5])
summary(myData)

itemFrequencyPlot(myData, topN = 10)

#apriori function
#if support is .5 is to high. We are asking for 50% of the transsactions
rules <- apriori(myData, parameter = list(minlen = 2,
                                          support = 0.003,
                                          confidence = 0.2))
# view the top 10 rules by lift
inspect(sort(rules, by = 'lift')[1:10])

plot(rules)
top10rules <- head(rules, n = 10, by = 'lift')
plot(top10rules, method = "graph", engine = "htmlwidget")

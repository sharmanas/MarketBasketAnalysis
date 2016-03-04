# Setting work directory
setwd("C:/Users/mxs67/Desktop/Lennox")
library(openxlsx)

# Reading Data files
consumer.old <- read.xlsx("2014 Sarah M.xlsx")
consumer.new <- read.xlsx("Registrations for Marketing.xlsx")
customer <- rbind(consumer.old, consumer.new)
# Creating function to split a character string
splitter = function(x) {
  unlist(strsplit(x, split="-"))[1]
}
customer$Model.No <- unlist(lapply(customer$Model_Nbr, splitter))
x <- substr(customer$Model.No, 1, 7)
customer$Model <- x
head(customer)

# Create data frame to implement MBA
mba <- data.frame(Transaction.ID = customer$Registration_Nbr, Product = customer$Model)
#write.csv(mba2014, "C:/Users/mxs67/Desktop/Lennox/MBA2014.csv", row.names = FALSE)
mba$Product <- as.character(mba$Product)
str(mba)
transactions <- split(mba$Product, mba$Transaction.ID)
head(transactions)
library(arules)
unique.transactions <- function(x) {
  for (i in 1:length(x)) {
    x[[i]] <- unique(x[[i]])
  }
  x
}
txns <- unique.transactions(transactions)
head(txns)

# Create an object containing transactions
txn <- as(txns, "transactions")
# Implement Apriori algorithm and inspect top 25 rules
basket.rules <- apriori(txn, parameter = list(sup = 0.005, conf = 0.01, target="rules"))
itemFrequencyPlot(txn, topN = 25)
inspect(basket.rules) 
basket.rules.broad <- apriori(txn, parameter = list(sup = 0.001, conf = 0.001, target="rules"))
inspect(head(sort(basket.rules.broad, by="lift"), 20))

# Plot rules having lift greater than 2
library(arulesViz)
plot(basket.rules.broad)
#top.lift <- sort(basket.rules.broad, decreasing = TRUE, na.last = NA, by = "lift")
top.lift <- subset(basket.rules, subset=lift>2)
top.lift.sorted <- sort(top.lift, decreasing=TRUE, na.last=NA, by="lift")
inspect(top.lift.sorted)
plot(top.lift.sorted, method="graph")
inspect(head(sort(basket.rules, by="lift"), 25))
plot(head(sort(basket.rules, by="lift"), 25), method = "graph")
#IS606 Group Assignment 2

#setwd("C:/Users/sgurung/Documents/GitHub/IS606_Collab_2")
setwd("~/Code/Masters/IS606/Collab2")

raw <- read.csv("group-project-2-raw-data.csv")
raw$Store <- as.factor(as.character(raw$Store))
raw$Product <- as.factor(as.character(raw$Product))
str(raw)

#create unique store-product combinations, 270 total
raw$StoreProduct <- paste(raw$Store, raw$Product)
raw$StoreProduct <- as.factor(raw$StoreProduct)
length(unique(raw$StoreProduct))

#remove negative stock store-product combos (potentially bad data?)
#5/270 store-product combos removed (70 rows)
raw[raw$InStock < 0,]
negStock <- unique(raw[raw$InStock<0,]$StoreProduct)
negStock <- factor(negStock)
raw2 <- subset(raw, !(StoreProduct %in% negStock))

#calculate demand as InStock(n-1) - Instock(n) + InTransit(n-1)
raw2$Demand <- numeric(nrow(raw2))
raw2[1,]$Demand <- NA
raw2$OnOrder <- NULL
for (i in 2:nrow(raw2)) {
  raw2[i,]$Demand <- 
    ifelse(raw2[i-1,]$StoreProduct == raw2[i,]$StoreProduct,
           raw2[i-1,]$InStock # previous day stock
         + ifelse(            # plus shipment received on current day
             raw2[i-1,]$InTransit > 0,
             (raw2[i-1,]$InTransit - raw2[i,]$InTransit + raw2[i-1,]$AtCenter),
             0)
         - raw2[i,]$InStock   # minus current day stock
         ,NA)
}
head(raw2,100)

#remove negative demand (could be illogical or need to capture center straight to store [i.e. no InTransit])
#49/265 store-product combos removed (686 rows)
x <- subset(raw2, Demand<0)
negDemand <- unique(x$StoreProduct)
negDemand <- factor(negDemand)
negDemand
raw3 <- subset(raw2, !(StoreProduct %in% negDemand))
raw3$StoreProduct <- factor(raw3$StoreProduct)

#calculate mean demand (lambda) per (216) Store-Product combos
library(plyr)
md<- aggregate(raw3$Demand, list(raw3$StoreProduct), mean, na.rm=TRUE)
colnames(md) <- c("StoreProduct", "meanDemand")

write.csv(md, "meanDemand.csv")

#------------------------

head(raw3)

# Grab the last row of each store to get the most recent InStock number
stock = ddply(.data=raw3, c("raw3$StoreProduct"), .fun=function(x) { 
  data.frame(
    x[nrow(x), c('Date','InStock')]
  )
})
head(stock)
head(md)

# Combine with computed lambda values for further analysis
stock = cbind(stock, md)
stock = stock[, c('StoreProduct', 'InStock','meanDemand')]
head(stock)


#
# New Code Follows
#

# This function will simulate a weekly demand and check if the demand is met by product in stock
# for each day of the week.
# Returns: a vector of length 7 (week) that contains 0 if demand was met and 1 otherwise.
#
daily_demand = function(lambda, stock){  
  cum_demand = cumsum(rpois(7, lambda))
  test = ifelse(cum_demand <= stock, 0, 1)
}

# test stub
#sim_result = replicate(10, daily_demand(1, 3))
#sim_result
#rowSums(sim_result)
#rowSums(sim_result)/10

# This block simulates the weekly demand N times and computes the probability of meeting the demand
# for each day.
# Result is a data frame with Stock information, and probability of running out of stock for each
# day of the week.
#
N = 10000
result = ddply(.data=stock, 1, .fun=function(x) { 
  N_weeks = replicate(N, daily_demand(x$meanDemand, x$InStock))
  p = rowSums(N_weeks)/N
  data.frame(
    x,
    P1=p[1],
    P2=p[2],
    P3=p[3],
    P4=p[4],
    P5=p[5],
    P6=p[6],
    P7=p[7]
  )
})
head(result)

# Probability First Day
firstDayResults <- result[result$P1 > 0, c('StoreProduct', 'P1')]
firstDayResults[with(firstDayResults, order(-P1)), ]

# Probability Second Day
secondDayResults <- result[result$P2 > 0, c('StoreProduct', 'P2')]
secondDayResults[with(secondDayResults, order(-P2)), ]

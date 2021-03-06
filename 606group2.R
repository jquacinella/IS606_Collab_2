#IS606 Group Assignment 2
#Brian Chu, Rohan Fray, Sharad Gurung, James Quacinella

#setwd("C:/Users/sgurung/Documents/GitHub/IS606_Collab_2")
#setwd("~/Code/Masters/IS606/Collab2")

raw <- read.csv("group-project-2-raw-data.csv")
raw$Store <- as.factor(as.character(raw$Store))
raw$Product <- as.factor(as.character(raw$Product))
str(raw)

#create unique store-product combinations, 270 total
raw$StoreProduct <- paste(raw$Store, raw$Product)
raw$StoreProduct <- as.factor(raw$StoreProduct)
length(unique(raw$StoreProduct))

#remove negative stock store-product combos (potentially bad data)
#5/270 store-product combos removed (70 rows)
raw[raw$InStock < 0,]
negStock <- unique(raw[raw$InStock<0,]$StoreProduct)
negStock <- factor(negStock)
raw2 <- subset(raw, !(StoreProduct %in% negStock))

#calculate demand as InStock(n-1) - Instock(n) + InTransit(n-1) [if delivered next day]
raw2$Demand <- numeric(nrow(raw2))
raw2[1,]$Demand <- NA
raw2$OnOrder <- NULL
for (i in 2:nrow(raw2)) {
  raw2[i,]$Demand <- 
    ifelse(raw2[i-1,]$StoreProduct == raw2[i,]$StoreProduct,
           raw2[i-1,]$InStock # previous day stock
           + ifelse(            # plus shipment received on current day
             (raw2[i-1,]$InTransit > 0) & ((raw2[i,]$InTransit == 0) | (raw2[i-1,]$InStock < raw2[i,]$InStock)),
             raw2[i-1,]$InTransit,
             0)
           - raw2[i,]$InStock   # minus current day stock
           ,NA)
}

head(raw2,100)

#remove negative demand (could be illogical or need to capture center straight to store [i.e. no InTransit])
#49/265 store-product combos removed (686 rows)
temp <- subset(raw2, Demand<0)
negDemand <- unique(temp$StoreProduct)
negDemand <- factor(negDemand)
negDemand
raw3 <- subset(raw2, !(StoreProduct %in% negDemand))
raw3$StoreProduct <- factor(raw3$StoreProduct)

#calculate mean demand (lambda) per (216) Store-Product combos
library(plyr)
md<- aggregate(raw3$Demand, list(raw3$StoreProduct), mean, na.rm=TRUE)
colnames(md) <- c("StoreProduct", "meanDemand")

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

# Print total results sorted by P1
print(result[with(result, order(-P1)), c('StoreProduct', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7')], row.names = FALSE)



### Part 4

part4 <- raw3[raw3$Date != '2014-04-08' & raw3$Date != '2014-04-21', ]
oneDay <- 0;
twoDays <- 0;
threeDays <- 0;
for (i in 1:nrow(part4)) {
  if (part4[i,]$InTransit != 0) {
    if (part4[i,]$InTransit == part4[i+1,]$InTransit && part4[i,]$InTransit == part4[i+2,]$InTransit) {
      threeDays <- threeDays + 1;      
    }
    else if (part4[i,]$InTransit == part4[i+1,]$InTransit) { 
      twoDays <- twoDays + 1;
    } 
    else { 
      oneDay <- oneDay + 1;
    }
  }
}
totalTransits = oneDay + twoDays + threeDays;
cat("Percentage of transit times that are one day: ", (oneDay / totalTransits)*100);
cat("Percentage of transit times that are at most two day: ", ((oneDay + twoDays) / totalTransits)*100);

#twoDays
#threeDays

result$StoreProduct <- reorder(result$StoreProduct, result$P2)
ggplot(result[result$P2 > .01, ],aes(StoreProduct,P2)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  ggtitle("Products with > 1% Chance of going out of stock in 48h") + 
  xlab("Store +  Product") + 
  ylab("Probability of Running Out of Stock in 48h")
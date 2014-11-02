#IS606 Group Assignment 2

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
for (i in 2:nrow(raw2)) {
  raw2[i,]$Demand <- ifelse(raw2[i-1,]$StoreProduct == raw2[i,]$StoreProduct, 
                       raw2[i-1,]$InStock - raw2[i,]$InStock + raw2[i-1,]$InTransit, NA)
}

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

#add poisson distribution variable (not working yet)
md$poisson <- numeric(nrow(md))
for (i in nrow(md)) {
  md[i,]$poisson <- ppois(1:7, lambda=md[i,]$meanDemand)[1]
}



stock = ddply(.data=raw3, c("raw3$StoreProduct"), .fun=function(x) { 
  data.frame(
    x[nrow(x), c('InTransit','InStock')]
  )
})

head(stock)
head(md)

stock = cbind(stock, md)
stock = stock[, c('StoreProduct', 'InTransit','InStock','meanDemand')]
head(stock)

sim_demand = ddply(.data=stock, 1, .fun=function(x) { 
  d = rpois(7, x$meanDemand)
  data.frame(
    x,
    D1=d[1],
    D2=d[2],
    D3=d[3],
    D4=d[4],
    D5=d[5],
    D6=d[6],
    D7=d[7],
    
    Total = sum(d)
  )
})
head(sim_demand)

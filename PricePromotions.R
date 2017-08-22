'PRICE AND PROMOTIONS - SIMULATING DATA POINTS - Several Countries and Stores'
'======================================='

library(ggplot2)
rm(list=ls())

# 'create data structure that will hold the data, a simulation of sales for the two products in 20
# 20 stores over 2 years with prices and promotions status'
# 
# k.stores <- 20
# k.weeks <- 52 * 2
# 
# store.df <- data.frame(matrix(NA,ncol = 10,nrow = k.stores*k.weeks))
# names(store.df) <- c("storeNum","Year","Week","p1sales","p2sales","p1price","p2price",
#                      "p1prom","p2prom","country")
# dim(store.df)
# 
# store.num <- 101:(100 + k.stores)
# 
# (store.cty <- c(rep("US",3),rep("DE",5),rep("GB",3),rep("BR",2),rep("JP",4),rep("AU",1),rep("CN",2)))
# length(store.cty)
# 
# store.df$storeNum <- rep(store.num,each=k.weeks)
# store.df$country <- rep(store.cty,each=k.weeks)
# 
# rm(store.num,store.cty)
# 
# (store.df$Week <- rep(1:52,times=k.stores*2))
# 
# (store.df$Year <- rep(rep(1:2,each=k.weeks/2),times = k.stores))
# 
# str(store.df)
# 
# 'Using factor in Store Number and Country'
# store.df$storeNum <- factor(store.df$storeNum)
# store.df$country <- factor(store.df$country)
# str(store.df)
# 
# 'generating randon data points for prices and promotions - 10% and 15% promoted'
# 'set.seed(2)'
# store.df$p1prom <- rbinom(n=nrow(store.df),size = 1,p=0.1)
# store.df$p2prom <- rbinom(n=nrow(store.df),size = 1,p=0.15)
# 
# 'generating randon data points for prices and promotions - price between intervals'
# store.df$p1price <- sample(x = c(2.19,2.29,2.49,2.79,2.99),size = nrow(store.df),replace=TRUE)
# store.df$p2price <- sample(x = c(2.29,2.49,2.59,2.99,3.19),size = nrow(store.df),replace=TRUE)
# 
# 'generating sales'
# 'create sales in the absence of promotions'
# tmp.sales1 <- rpois(nrow(store.df),lambda = 120)
# tmp.sales2 <- rpois(nrow(store.df),lambda = 100)
# 
# store.df
# 
# tmp.sales1 <- tmp.sales1 * log(store.df$p2price/log(store.df$p1price))
# tmp.sales2 <- tmp.sales2 * log(store.df$p1price/log(store.df$p2price))
# 
# 'final sales get a 30% or 40% lift when promoted'
# store.df$p1sales <- floor(tmp.sales1 * (1 + store.df$p1prom * 0.3))
# store.df$p2sales <- floor(tmp.sales2 * (1 + store.df$p2prom * 0.4))


#=============================================================================


setwd("~/Desktop")
write.csv(store.df,file = "TS-PP.csv")
PP <- read.csv("TS-PP.csv",sep = ",",header = T)


'Histogram'
hist(PP$p1sales,
     main = "Product 1 Weekly Sales Frequencies, All Stores",
     xlab = "Product 1 Sales (Units)",
     ylab = "Relative Frequency",
     breaks = 30,
     col="lightblue",
     freq = FALSE,
     xaxt="n")
axis(side = 1,at=seq(60,300,by=20))
lines(density(store.df$p1sales,bw=10),type="l",col="darkred",lwd=2)

'BoxPlot'
boxplot(PP$p2sales, xlab="Weekly sales", ylab="P2",main="Weekly Sales of P2, All Stores", horizontal = TRUE)
boxplot(PP$p2sales ~ PP$storeID, horizontal=TRUE, ylab="Store",xlab="Weekly Unit Sales", las=1,main="Weekly Sales of P2 by Store")
boxplot(p2sales ~ p2prom, data=PP, horizontal=TRUE,yaxt="n",ylab="P2 promoted in Store?",xlab="Weekly Sales",
        main="Weekly Sales of P2 with and without Promotion")
axis(side = 2, at=c(1,2),labels = c("No","Yes"))

'ggplot of box plot'

ggplot(PP,aes(p1sales,fill = storeID)) + geom_histogram() +
  facet_wrap(~storeID,ncol = 1)



'QQ Plot to Check Normality'
qqnorm(store.df$p1sales)
qqline(store.df$p1sales)

qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))

plot(ecdf(store.df$p1sales),
     main = "Cumulative Distribution of P1 Weekly Sales",
     ylab = "Cumulative Proportion",
     xlab = c("P1 Weekly Sales, all Stores", "90% of Weeks sold <= 171 units"),
     yaxt = "n")

axis(side = 2, at=seq(0,1, by=0.1), las=1 , labels = paste(seq(0,100,by=10),"%",sep = ""))
abline(h=0.9,lty=3)
abline (v=quantile(store.df$p1sales,pr=0.9),lty=3)

'Aggregate by'
by(store.df$p1sales,store.df$storeNum,mean)
by(store.df$p1sales,list(store.df$storeNum,store.df$Year),mean)

test<-aggregate(PP$p1sales ~ PP$Week,data = PP,FUN = sum)


#'plot with weekly sales and Units sold for One Country'
UnitsVolume1Chart <- aggregate(store.df$p1sales ~ store.df$Week,FUN=sum,subset = store.df$country=='US')
plot(UnitsVolume1Chart,xlab = "Weeks",ylab="Sales of Units",type="l",main="Weekly Units Sold",
     ylim=c(min(UnitsVolume1Chart$`store.df$p1sales` - 100),
            max(UnitsVolume1Chart$`store.df$p1sales` + 50)))


#'Create Price Difference and Price Elasticity'
store.df$PriceDiff <- store.df$p1price - store.df$p2price
AVGVol1<-mean(store.df[store.df$country == 'US', ]$p1sales)
  
  

AVGPrice1 <- mean(store.df[store.df$country == 'US',]$p1price)

US <- subset(store.df,store.df$country=='US')
head(US)
US$El <- (US$PriceDiff/AVGVol1)*AVGPrice1

#PLOT with GGPLOT===================================================================

plot.data <- data.frame(
  week = US$Week,
  sales.volume = c(US$p1sales, US$p2sales),
  brand = c(rep("p1", nrow(US)), rep("p2", nrow(US)))
)

tail(plot.data)

Example <- ggplot(plot.data, aes(week, sales.volume, color=brand)) +
  geom_line() +
  geom_line(data=US, aes(Week, (El + 0.03)/0.0005), color="green") +
  scale_y_continuous(sec.axis = sec_axis(~.*0.0005 - 0.03, name = "Elasticity"))

Example



'BoxPlot'
boxplot(PP$p2sales, xlab="Weekly sales", ylab="P2",main="Weekly Sales of P2, All Stores", horizontal = TRUE)
boxplot(PP$p2sales ~ PP$storeNum, horizontal=TRUE, ylab="Store",xlab="Weekly Unit Sales", las=1,main="Weekly Sales of P2 by Store")
boxplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE,yaxt="n",ylab="P2 promoted in Store?",xlab="Weekly Sales",
        main="Weekly Sales of P2 with and without Promotion")
axis(side = 2, at=c(1,2),labels = c("No","Yes"))

















#Question 1
#Fetch data of the last 2 years of NASDAQ Composite 
#from yahoo finance
internetData <-read.csv("http://chart.finance.yahoo.com/table.csv?s=^IXIC&a=3&b=28&c=2015&d=3&e=28&f=2017&g=d&ignore=.csv")
#View the fetched data
View(internetData)

#Question 2
#Daily Returns for the past one year
returns = internetData[1:253,"Adj.Close"]
#View the daily returns for the past year
View(data.frame(diff(as.matrix(returns))))


#Question 3
#Graphically represent the stock prices as a line plot
stockClosingprice = internetData$Close
plot(stockClosingprice,type="o",col="red", main="Stock Prices of NASDAQ",
     xlab = "Index",
     ylab = "Prices of Stock"
     )


#Question 4
#Bucket the daily return values into bins and plot a histogram
hist(returns,
     main = "Histogram of Daily Return Values of NASDAQ Prices \n28-04-2016 to 27-04-2017",
     xlab = "Daily Returns",
     breaks = 50,
     col=heat.colors(6)
     )

#Question 5
#Calculate mean, median and standard deviation of Daily return values

#mean
mean(returns) #5332.417
#median
median(returns) #5269.15
#standard deviation
sqrt(var(returns)) #361.9824

#plot them on the same graph mentioned in step IV
hist(returns,
     main = "Histogram of Daily Return Values of NASDAQ Prices \n28-04-2016 to 27-04-2017",
     xlab = "Daily Returns",
     breaks = 50,
     col=heat.colors(6),
     probability = TRUE
)
lines(density(returns),
      lwd = 4,
      col = "black")
abline(v=mean(returns), col= "green",lwd = 3)
abline(v=median(returns), col= "blue",lwd = 3)
abline(v=sqrt(var(returns)), col= "purple",lwd = 3)
legend(x = "topleft",
       c("Density plot",
         "Mean", 
         "Median",
         "SD"
        ),
       col=c("black","green","blue","purple"),
       lwd = c(4,3,3,3)
       )

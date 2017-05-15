wknDir <- "C:\\data\\ORCL"# working directory
setwd(wknDir)#change working directory

#Load all the required data
yahoo <- read.csv("YHOO.csv", header = TRUE)
ebay <- read.csv("EBAY.csv", header = TRUE)
oracle <- read.csv("ORCL.csv", header = TRUE)

#QUESTION 1
#Load data of the last 1 years of ORCL

#SOLUTION 1
View(oracle)


#QUESTION 2
#Monthly returns of downloaded stock over the period under study

#SOLUTION 2
monthlyReturn <- function(stockData){  #function to calculate monthly returns
  returns = stockData["Adj.Close"]
  data.frame(diff(as.matrix(returns)))
}
returnORCL <- monthlyReturn(oracle)
View(returnORCL)    #Oracle Monthly Returns



#QUESTION 3
#Using a combination function, 
#calculate the monthly returns of an equally
#weighted portfolio consisting of any 3
#of the five stocks in question

#SOLUTION
#Calculate returns for 3 stock portfolio
View(returnORCL)  #returns for oracle
returnEBAY <- monthlyReturn(ebay)
View(returnEBAY) #returns for ebay
returnYAHOO <- monthlyReturn(yahoo)
View(returnYAHOO) #returns for yahoo
#Weigh portfolio of 3 or 5 stock


#QUESTION 4
#Graphically represent the cumulative monthly 
#returns of each of the possible portfolios through line plots

#SOLUTION
#A function to calculate cummulative monthly returns
cummulativeMonthlyReturns <- function(returns){
  n <- nrow(returns)
  return(
    (returns[2:n,1] - returns[1:(n-1), 1])/returns[1:(n-1),1]
  )
}
#Cummulate returns for Oracle
o <- cummulativeMonthlyReturns(returnORCL)
#Cummulate returns for yahoo
y <- cummulativeMonthlyReturns(returnYAHOO)
#Cummulate returns for ebay
e <- cummulativeMonthlyReturns(returnEBAY)
#Ploting the portfolios on a graph
plot(y, type = "o",
     ylim=c(-12,7),
     col = "orange", 
     ylab = "Returns",
     xlab = "Months",
     col.lab="blue",
     main = "Cummulative Monthly Returns of \n Oracle, Yahoo & Ebay portfolios",
     lwd=3
     )
lines(o, type = "o", col = "blue", lwd=3)
lines(e, type = "o", col = "red", lwd=3)
abline(h=0, col="black")
legend(
  "topright",
  lty = c(1,1,1),
  col = c("blue","orange", "red"),
  legend = c("Oracle", "Yahoo", "Ebay")
)

#QUESTION 5
#Calculate mean, median and standard deviation
#of monthly values for each of the portfolios 
#in question and plot them on the same graph 
#mentioned in step 4.

#SOLUTION
#Mean, Median and SD of Oracle
plot(o, type = "o",
     ylim=c(-12,7),
     xlim=c(-2,8),
     col = "blue", 
     ylab = "Returns",
     xlab = "Months",
     col.lab="blue",
     main = "Mean, Median & SD of \n Oracle",
     lwd=3
)
legend(
  "topright",
  lty = c(1,1,1),
  col = c("blue"),
  legend = c("Oracle")
)
abline(v=mean(o), col= "green",lwd = 2,lty=2)
abline(v=median(o), col= "blue",lwd = 2,lty=2)
abline(v=sqrt(var(o)), col= "purple",lwd = 2,lty=2)
legend(
  "topleft",
  lty = c(2,2,2),
  col = c("green","blue", "purple"),
  legend = c("Mean", "Median", "SD")
)
#Mean, Median and SD of Yahoo
plot(y, type = "o",
     ylim=c(-12,7),
     xlim=c(-2,8),
     col = "orange", 
     ylab = "Returns",
     xlab = "Months",
     col.lab="blue",
     main = "Mean, Median & SD of \n Yahoo",
     lwd=3
)
legend(
  "topright",
  lty = c(1,1,1),
  col = c("orange"),
  legend = c("Yahoo")
)
abline(v=mean(y), col= "green",lwd = 2,lty=2)
abline(v=median(y), col= "blue",lwd = 2,lty=2)
abline(v=sqrt(var(y)), col= "purple",lwd = 2,lty=2)
legend(
  "topleft",
  lty = c(2,2,2),
  col = c("green","blue", "purple"),
  legend = c("Mean", "Median", "SD")
)
#Mean, Median and SD of Ebay
plot(e, type = "o",
     ylim=c(-12,7),
     xlim=c(-2,8),
     col = "red", 
     ylab = "Returns",
     xlab = "Months",
     col.lab="blue",
     main = "Mean, Median & SD of \n EBay",
     lwd=3
)
legend(
  "topright",
  lty = c(1,1,1),
  col = c("red"),
  legend = c("EBay")
)
abline(v=mean(y), col= "green",lwd = 2,lty=2)
abline(v=median(y), col= "blue",lwd = 2,lty=2)
abline(v=sqrt(var(y)), col= "purple",lwd = 2,lty=2)
legend(
  "topleft",
  lty = c(2,2,2),
  col = c("green","blue", "purple"),
  legend = c("Mean", "Median", "SD")
)



#Question 6
#Calculate the overall variance of all portfolio returns
#Solution
overallVariance = (var(y)+var(e)+var(o))/3
overallVariance #8.863085


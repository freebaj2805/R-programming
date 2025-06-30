########################## Package Installation######################
install.packages("datarium")
install.packages("qqplotr")
install.packages("ggplot2")
install.packages("RVAideMemoire")
install.packages("corrplot")
install.packages("TTR")
install.packages("forecast")


############### Library Installation #################3
library(corrplot)
library(ggplot2)
library(datarium)
library(qqplotr)
library(RVAideMemoire)
library(Hmisc)
library(TTR)
library(forecast)

# importing Data
Economy <- read.csv("jobs.csv",header = TRUE)
GDP <- read.csv("Book1.csv",header = TRUE)

#checking  the statistics of data 
head(GDP)
summary(GDP)

ggplot(mapping = aes(sample = GDP$Exports_of_goods_GDP)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

set.seed(10)
shapiro.test(GDP$Exports_of_goods_GDP)

# Plot the distribution of the Export of goods and service
hist(GDP$Exports_of_goods_GDP)

################# Test for Hypothesis #######################

# Statistical mean,Median,standard deviation
mean(GDP$Exports_of_goods_GDP)
sd(GDP$Exports_of_goods_GDP)
median(GDP$Exports_of_goods_GDP)

# performing a One tail Hypothesis test 
t.test(GDP$Exports_of_goods_GDP, mu=29, alternative="less")

# perfomimgm ANOVA test to compare foreing net inflow on GBD
boxplot(Foreign_net_inflows ~ Country_Name , data= Economy, names=c("Canada", "France", "China","Germany", "Greece","Japan" , "Quatar","United State", "United Kingdom", "Australia"),
        xlab="Countries", ylab="inflation",main="inflation net inflow ")

#Recalling the Shapiro wilk test 
byf.shapiro(Foreign_net_inflows ~ Country_Name , data=Economy)

# cheking homogienity
bartlett.test(Foreign_net_inflows ~ Country_Name , data=Economy)

# performing the ANOVA test
oneway.test(Foreign_net_inflows ~ Country_Name , data=Economy, var.equal = TRUE)


############## REGRESSION #######################

#checking all data types of the economy table
head(as.data.frame(Economy))
str(Economy)

# Reducing the tables to contain only numeric values 
Economy_Reg <- Economy[ ,c("Inflation", "Exports_of_goods_and._services", "Foreign_net_inflows",
                           "Foreign_net.outflows","Gross_capital_formation","Imports_of_goods_and_services")]

# checking & viewing correlation matrix 
cor(Economy_Reg)
corrplot(cor(Economy_Reg))

# Forward Stepwise regression analysis 
EREG <-lm(Exports_of_goods_and._services ~ Imports_of_goods_and_services, Economy_Reg)
summary.lm(EREG)

# visualizing the fitted regression line 
plot(Exports_of_goods_and._services ~ Imports_of_goods_and_services, Economy_Reg,
     col = "Orange",
     main = "Regression: Export of goods and services & import of goods and services ",
     xlab = "Export",
     ylab = "Import")

# regression line 
abline(EREG, col="Blue")

# checking residual
plot(EREG, 1)
# normality of residual
plot(EREG, 2)
#Equal variance of Homoscedasticity
plot(EREG, 3)

#################### CORRELATION ###########################

# correlation between all variables rounded to 3 decimal 
round(cor(Economy_Reg), digits = 3)

# Visualizing Correlation between import,export,and inflation
pairs(Economy_Reg[, c("Exports_of_goods_and._services", "Imports_of_goods_and_services","Inflation")])

###################### Time Series #################################

#Reading the data for time series 
Inflation <- read.csv("Time.csv",header = TRUE)

# Creating a Data Frame for the Inflation data
Inflation1 <- ts(Inflation, frequency=12, start=c(2006,1))
Inflation1

# Plotting a time series graph on inflation 
plot.ts(Inflation1)

####################### Decomposing seasonal data  #################################

# Decomposing inflation time series 
Dec_Inflation <- decompose(Inflation1)


# Estimating values of the Decomposed series
Dec_Inflation$seasonal
Dec_Inflation$trend
Dec_Inflation$random

# plotting the Decomposed Graph
plot(Dec_Inflation)
plot(Dec_Inflation$trend)
plot(Dec_Inflation$seasonal)
plot(Dec_Inflation$random)


########################## Seasonal Adjustment ####################

# adjusting  inflation time series 
Adj_Inflation <- Inflation1 - Dec_Inflation$seasonal

#plotting the  graph Adjusted time series 
plot(Adj_Inflation)

####################### Forecasting time series ######################

# smoothing and forecasting time series
For_Inflation <- HoltWinters(Inflation1, beta=FALSE, gamma=FALSE)
For_Inflation

# to get the fitted values 
For_Inflation$fitted

# plotting the fitted over the time series 
plot(For_Inflation)

# calculating the sum of Square error
For_Inflation$SSE

# forecasting with the initial values of the set 
HoltWinters(For_Inflation, beta=FALSE, gamma=FALSE, start = 1.46)

# forecast for the next 10 months 
For_Inflation1 <- forecast(For_Inflation, h=10)
For_Inflation1

# plotting the forecasting graph
plot(For_Inflation1)

# Plotting a Correlogram
acf(For_Inflation1$residuals, lag.max=20 , na.action = na.pass)

# Box test
Box.test(For_Inflation1$residuals, lag=20, type="Ljung-Box")
# checkinf forcast error
plot.ts(For_Inflation1$residuals)

## forecast error normally didtributed 
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4 
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

# removing all null values and ploting an histogram  
For_Inflation1$residuals <- For_Inflation1$residuals[!is.na(For_Inflation1$residuals)]
plotForecastErrors(For_Inflation1$residuals)







library(readxl)
library(ZIM)


setwd("C:/Users/nazar\Desktop/Classes/Forecasting/Assignment/data")
data <- read_excel("DataSets2022.xlsx", sheet="Airpass_BE")
Air <- ts(data[,2], frequency = 12, start = 2003)

# Time series plot
plot(Air, main="Timeseries plot: Air passengers - BE Intra-EU",
     xlab="Year", ylab="Number of Passengers")


# Seasonal plot
par(mai = c(0.5, 0.5, 0.5, 0.05))
seasonplot(Air,year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot: Air passengers - BE Intra-EU",
           ylab="Number of Passengers",col=rainbow(20), pch=19)

# Seasonal Subseries
monthplot(Air,ylab="Number of Passengers",xlab="Month", xaxt="n",
          main="Seasonal subseries plot: Air passengers - BE Intra-EU",
          type="l")
axis(1, at=1:12, labels=month.abb, cex=0.8)


# Split the data in training and test set
data2 <- data[1:206,]
Air2 <- ts(data2[,2], frequency = 12, start = 2003)
air_train <- window(Air2, end=2017)
air_test  <- window(Air2, start=2018)

# Data tranformation

plot(BoxCox(Air2,lambda=1),main="No Transformation")

## ?? = 1/2 Square root plus linear transformation
plot(BoxCox(Air2,lambda=1/2),main="Square root plus linear transformation")

## ?? = 0 Natural logarithm
plot(BoxCox(Air2,lambda=0), main="Natural logarithm Transformation")

## ?? = -1 Inverse Transformation
plot(BoxCox(Air2,lambda=-1), main="Inverse Transformation")

## ?? = -1 cube root Transformation
plot(BoxCox(Air2,lambda=1/3), main="Cube Root Transformation")

## Ultimate Lambda
l <- BoxCox.lambda(Air2)
l
## Plot with ultimate
plot(BoxCox(Air2,lambda=0.01461759), main="Transformation using ultimate lambda")

#saving the length of test in a variable
h = length(air_test)
h
# Seasonal Naive method
m1 <- snaive(air_train, h=h, lambda=l)
plot(m1, xlim=c(2003,2020), ylab="Number of Passengers", xlab = "Year")

# Ljung-Box Test
res <- residuals(m1)
Box.test(res, lag=h, fitdf=0, type="Lj")
checkresiduals(m1)


# Accuracy
accuracy(m1, air_test)

#STL Decomposition
m2 <- stlf(air_train, method="naive", h=h, lambda = l, biasadj = TRUE)
m3 <- stlf(air_train, method="rwdrift", h=h, lambda = l, biasadj = TRUE)
m4 <- stlf(air_train, method="ets", h=h, lambda = l, biasadj = TRUE)
m5 <- stlf(air_train, method="arima", h=h, lambda = l, biasadj = TRUE)

## Creating Table of accuracies
# list of models
models <- c('Seasonal naive', 'stl_Na�ve', 'stl_rwdrift', 'stl_ets',
            'stl_arima', 'ets_AAN', 'ets_AADN',
            'ets_AAA', 'ets_AADA', 'ets', 'arima1', 'arima2', 'arima3', 'arima4')

n <- length(models); n   #number of models

#naming of models for the given data set
m <- "m"

#naming of training and test set
trainset <- air_train
testset <- air_test

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

#for loop to collect measures
for(i in 1:n) {
  #accuracy measures
  assign(paste0("a_m", i), accuracy(get(paste0(m, i)), testset)[,c(2,3,5,6)])
  a_train <- rbind(a_train, c(i, get(paste0("a_m", i))[1,]))
  a_test <- rbind(a_test, c(i, get(paste0("a_m", i))[2,]))
  
  #residual diagnostics  
  assign(paste0("res_m", i), checkresiduals(get(paste0(m, i)), plot = FALSE))
  res_matrix[i,] <- c(i, get(paste0("res_m", i))$statistic, 
                      get(paste0("res_m", i))$parameter, 
                      get(paste0("res_m", i))$p.value)
}

rownames(a_train) <- models
colnames(a_train)[1] <- "nr"
rownames(a_test) <- models
colnames(a_test)[1] <- "nr"

a_train
a_test
round(res_matrix, digits = 4)

write.csv(a_test,"C:/Users/nazar/Desktop/Forecasting/acc4.csv" )
write.csv(res_matrix,"C:/Users/nazar/Desktop/Forecasting/res4.csv")

# ETS models
m6_t <- ets(air_train, model = "AAN", damped = FALSE, lambda = l)
m6 <- forecast(m6_t, h=h)
m7_t <- ets(air_train, model = "AAN", damped = TRUE, lambda = l)
m7 <- forecast(m7_t, h=h)
m8_t <- ets(air_train, model = "AAA", damped = FALSE, lambda = l, biasadj = TRUE)
m8 <- forecast(m8_t, h=h)
m9_t <- ets(air_train, model = "AAA", damped = TRUE, lambda = l, biasadj = TRUE)
m9 <- forecast(m9_t, h=h)
m10_t <- ets(air_train, lambda = l)
m10 <- forecast(m10_t, h=h)


checkresiduals(m10, plot=FALSE)

plot(m10,  xlim=c(2003,2020), ylab="Number of Passengers", xlab = "Year")


#Auto-arima
m11_t <- auto.arima(air_train, stepwise = FALSE, approximation = FALSE)
m11 <- forecast(m11_t, h=h)
m12_t <- auto.arima(air_train, stepwise = FALSE, approximation = FALSE, lambda = l, biasadj = TRUE)
m12 <- forecast(m12_t, h=h)
m13_t <- auto.arima(air_train, stepwise = FALSE, approximation = FALSE, d=1, D=1)
m13 <- forecast(m13_t, h=h)
m14_t <- auto.arima(air_train, stepwise = FALSE, approximation = FALSE, lambda = l, biasadj = TRUE, d=1, D=1)
m14 <- forecast(m14_t, h=h)


## Final Forecast
m_final <- ets(Air2, lambda = l)
forecast_final <- forecast(m_final, h=34)
plot(forecast_final, ylab="Number of Passengers", xlab = "Year" , xlim=c(2003,2025), ylim = c(0,3200000))


lines(window(Air,start=c(2020,2)),col="red")


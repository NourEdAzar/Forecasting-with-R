
library(readxl)

#Reading the data
setwd("C:/Users/nazar/Desktop/Classes/Forecasting/Assignment/data")
data <- read_excel("IPG3113N.xls")
candy <- ts(data[,2], frequency = 12, start = 1972)

# Time series plot
plot(candy, main="Timeseries plot: Monthly Candy Consumption",
     xlab="year", ylab="index")

# Seasonal plot
seasonplot(candy,year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot: Monthly Candy Consumption",
           ylab="index",col=rainbow(20), pch=19)

# Seasonal Subseries
monthplot(candy,ylab="index",xlab="Month",
          main="Seasonal subseries plot: Monthly Candy Consumption",
          type="l")
axis(1, at=1:12, labels=month.abb, cex=0.8)


# Split the data in training and test set
candy_train <- window(candy, end=2014)
candy_test  <- window(candy, start=2015)

# Data tranformation
## Ultimate Lambda
l <- BoxCox.lambda(candy)
l
## Plot with ultimate
plot(BoxCox(candy,lambda=l), main="Transformation using ultimate lambda", ylab="index",xlab="Month")

#saving the length of test in a variable
h = length(candy_test)
h

# Seasonal Naive method
m1 <- snaive(candy_train, h=h, lambda=l)

# Stl decomposition
m2 <- stlf(candy_train, method="naive", h=h, lambda = l, biasadj = TRUE)
m3 <- stlf(candy_train, method="rwdrift", h=h, lambda = l, biasadj = TRUE)
m4 <- stlf(candy_train, method="ets", h=h, lambda = l, biasadj = TRUE)
m5 <- stlf(candy_train, method="arima", h=h, lambda = l, biasadj = TRUE)

# ETS models
m6_t <- ets(candy_train, model = "AAN", damped = FALSE, lambda = l)
m6 <- forecast(m6_t, h=h)
m7_t <- ets(candy_train, model = "AAN", damped = TRUE, lambda = l)
m7 <- forecast(m7_t, h=h)
m8_t <- ets(candy_train, model = "AAA", damped = FALSE, lambda = l, biasadj = TRUE)
m8 <- forecast(m8_t, h=h)
m9_t <- ets(candy_train, model = "AAA", damped = TRUE, lambda = l, biasadj = TRUE)
m9 <- forecast(m9_t, h=h)
m10_t <- ets(candy_train)
m10 <- forecast(m10_t, h=h)
m10_t
#Auto-arima
m11_t <- auto.arima(candy_train, stepwise = FALSE, approximation = FALSE)
m11 <- forecast(m11_t, h=h)
m12_t <- auto.arima(candy_train, stepwise = FALSE, approximation = FALSE, lambda = l, biasadj = TRUE)
m12 <- forecast(m12_t, h=h)
m13_t <- auto.arima(candy_train, stepwise = FALSE, approximation = FALSE, d=1, D=1)
m13 <- forecast(m13_t, h=h)
m14_t <- auto.arima(candy_train, stepwise = FALSE, approximation = FALSE, lambda = l, biasadj = TRUE, d=1, D=1)
m14 <- forecast(m14_t, h=h)


## Creating Table of accuracies
# list of models
models <- c('Seasonal naive', 'stl_Na�ve', 'stl_rwdrift', 'stl_ets',
            'stl_arima', 'ets_AAN', 'ets_AADN',
            'ets_AAA', 'ets_AADA', 'ets', 'arima1', 'arima2', 'arima3', 'arima4')

n <- length(models); n   #number of models

#naming of models for the given data set
m <- "m"

#naming of training and test set
trainset <- candy_train
testset <- candy_test

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

write.csv(a_test,"C:/Users/nazar/Desktop/Forecasting/accex2.csv" )
write.csv(res_matrix,"C:/Users/nazar/Desktop/Forecasting/resex2.csv")


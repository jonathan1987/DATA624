library(readxl)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(GGally)
library(gridExtra)
library(forecast)
library(imputeTS)
library(tidyverse)
library(kableExtra)
s02_data <- read_xls("Set for Class.xls", n_max = 9732)
# For my group, I requested to look at only the Series = 's02'. Per assignment,
# you forecast Var02 and Var03 for S02

# Extract only seriesid, group, var02 and var03
s02_data <- s02_data %>% filter(group == "S02") %>%
  select("SeriesInd", "group", "Var02", "Var03")

summary(s02_data)
# Type Conversions. Change var02/03 to be time series
s02_data_var02_ts <- ts(s02_data$Var02, start = 1, end = 1622,
                        frequency=1)
s02_data_var03_ts <- ts(s02_data$Var03, start = 1, end = 1622,
                        frequency=1)
s02_data_var03_ts <- na.interpolation(s02_data_var03_ts, option = "spline")
# remove the large outlier the value == 38.28 per the summary
idx_outlier_var03 <- which.max(s02_data_var03_ts)
s02_data_var03_ts[idx_outlier_var03] <- NA
# time series plot of var02
var02_plot <- autoplot(s02_data_var02_ts) +
  ggtitle("S02 Var02 Time Series") +
  ylab("Var02")

# time series plot of var03
var03_plot <- autoplot(s02_data_var03_ts) + 
  ggtitle("S02 Var03 Time Series") +
  ylab("Var03")

# 2x1 plot arrangement
grid.arrange(var02_plot, var03_plot)
ggAcf(s02_data_var02_ts)
ggAcf(s02_data_var03_ts)
# naive forecasts and predict 140 steps ahead with 80% confidence invterval

var02_window_training <- window(s02_data_var02_ts, start=1, end=floor(1622*0.7))
var02_window_test <- window(s02_data_var02_ts, start=floor(1622*0.7))

var03_window_training <- window(s02_data_var03_ts, start=1, end=floor(1622*0.7))
var03_window_test <- window(s02_data_var03_ts, start=floor(1622*0.7))

# train a naive forecast using training data
s02_var02_naive_test_train <- naive(var02_window_training,
                                    h = length(var02_window_test), level = c(80, 90))
s02_var03_naive_test_train <- naive(var03_window_training,
                                    h = length(var03_window_test), level = c(80, 90))

# forecasts using naive method using the test windows/values
s02_var02_naive_test_fit <- naive(s02_data_var02_ts, h = 140, level = c(80, 90))
s02_var03_naive_test_fit <- naive(s02_data_var03_ts, h = 140, level = c(80, 90))

# forecast values using forecast()

checkresiduals(s02_var02_naive_test_fit)
checkresiduals(s02_var03_naive_test_fit)
# var02 plot
autoplot(s02_var02_naive_test_fit) +
  autolayer(s02_var02_naive_test_train, series="Naive") +
  autolayer(s02_data_var02_ts, series="Naive") +
  ggtitle("S02 Var02 Forecasts via Naive Forecasting") +
  xlab("") + ylab("")

# var02 plot
autoplot(s02_var03_naive_test_fit) +
  autolayer(s02_var03_naive_test_train, series="Naive") +
  autolayer(s02_data_var03_ts, series="Naive") +
  ggtitle("S02 Var03 Forecasts via Naive Forecasting") +
  xlab("") + ylab("")
# make holt predictions using the training data
s02_var02_holt_test_train <- holt(var02_window_training, 
                                  h = length(var02_window_test), level=c(80,90))
s02_var03_holt_test_train <- holt(var03_window_training, 
                                  h = length(var03_window_test), level=c(80,90))

# forecasts using naive method using the test windows/values
s02_var02_holt_test_fit <- holt(s02_data_var02_ts, h = 140, level=c(80,90))
s02_var03_holt_test_fit <- holt(s02_data_var03_ts, h = 140, level=c(80,90))

checkresiduals(s02_var02_holt_test_fit)
checkresiduals(s02_var03_holt_test_fit)
# var02 plot
autoplot(s02_var02_holt_test_fit) +
  autolayer(s02_var02_holt_test_train, series="Holt") +
  autolayer(s02_data_var02_ts, series="Holt_test_data") +
  ggtitle("S02 Var02 Forecasts via Holt's Method") +
  xlab("") + ylab("")


# var02 plot
autoplot(s02_var03_holt_test_fit) +
  autolayer(s02_var03_holt_test_train, series="Holt") +
  autolayer(s02_data_var03_ts, series="Holt_test_data") +
  ggtitle("S02 Var03 Forecasts via Holt's Forecasting") +
  xlab("") + ylab("")
# train an arima model using the training data
# var02 not seasonal but more of a trend
s02_var02_arima_train <- auto.arima(var02_window_training, seasonal = FALSE)
s02_var03_arima_train <- Arima(var03_window_training, order = c(2,0,1))

# make forecasts of the training data using arima models
s02_var02_arima_fit <- forecast(s02_var02_arima_train, h=length(var02_window_test))
s02_var03_arima_fit <- forecast(s02_var03_arima_train, h=length(var03_window_test))

# forecast on the test data for arima
s02_var02_arima_test <- auto.arima(s02_data_var02_ts, seasonal = FALSE) %>%
  forecast(h=140)
s02_var03_arima_test <- Arima(s02_data_var03_ts, order = c(2,0,1), seasonal = FALSE) %>%
  forecast(h=140)

# stl decomposition

checkresiduals(s02_var02_arima_test)
checkresiduals(s02_var03_arima_test)

# var02 plot
autoplot(s02_var02_arima_test) +
  autolayer(s02_var02_arima_fit, series="ARIMA forecast of test data") +
  autolayer(s02_data_var02_ts, series="ARIMA Forecast Values") +
  ggtitle("S02 Var02 Forecasts via ARIMA Method") +
  xlab("") + ylab("")


# var02 plot
autoplot(s02_var03_arima_test) +
  autolayer(s02_var03_arima_fit, series="ARIMA forecast of test data") +
  autolayer(s02_data_var03_ts, series="ARIMA Forecast Values") +
  ggtitle("S02 Var03 Forecasts via ARIMA Forecasting") +
  xlab("") + ylab("")
# ARIMA
arima_accuracy_var02 <- 
  data.frame(accuracy(s02_var02_arima_fit, var02_window_test))[2, "MAPE"]
arima_accuracy_var03 <- 
  data.frame(accuracy(s02_var03_arima_fit, var03_window_test))[2, "MAPE"]
holt_accuracy_var02 <- data.frame(accuracy(forecast(s02_var02_holt_test_train,
                                                    h=length(var02_window_test)), var02_window_test))[2, "MAPE"]

holt_accuracy_var03 <- data.frame(accuracy(forecast(s02_var03_holt_test_train,
                                                    h=length(var03_window_test)), var03_window_test))[2, "MAPE"]

naive_accuracy_s02 <- data.frame(accuracy(forecast(s02_var02_naive_test_train,
                                                   h=length(var02_window_test)), var02_window_test))[2, "MAPE"]

naive_accuracy_s03 <- data.frame(accuracy(forecast(s02_var03_naive_test_train,
                                                   h=length(var03_window_test)), var03_window_test))[2, "MAPE"]
var02_mape <- data.frame(Method=c("ARIMA", "Holt", "Naive"),
                         Value=c(arima_accuracy_var02,
                                 holt_accuracy_var02, 
                                 naive_accuracy_s02))
kable(var02_mape) %>% kable_styling(fixed_thead = T)
var03_mape <- data.frame(Method=c("ARIMA", "Holt", "Naive"),
                         Value=c(arima_accuracy_var03,
                                 holt_accuracy_var03,
                                 naive_accuracy_s03))
kable(var03_mape) %>% kable_styling(fixed_thead = T)
predictions_var02 <- s02_var02_arima_test$mean
write.csv(round(predictions_var02), "s02_var02_forecasts.csv")
predictions_var03 <- s02_var03_arima_test$mean
write.csv(round(predictions_var03, digits = 3), "s02_var03_forecasts.csv")

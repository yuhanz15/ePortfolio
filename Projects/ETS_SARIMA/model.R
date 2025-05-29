library(tidyverse)
library(xml2)
library(rvest)
library(forecast)
library(tseries)
library(ggplot2)

# data cleaning, original data is in xml file
target_row_index <- 4

xls_files <- list.files(path="data", pattern = "\\.xls$", full.names = TRUE)

results <- list()

for (file in xls_files) {
  doc <- read_xml(file)
  rows <- xml_find_all(doc, ".//ss:Row", xml_ns(doc))
  
  if (length(rows) >= target_row_index) {
    row <- rows[[target_row_index]]
    cells <- xml_find_all(row, ".//ss:Cell/ss:Data", xml_ns(doc))
    texts <- xml_text(cells)
    
    result_row <- c(file = basename(file), texts)
    results[[length(results) + 1]] <- result_row
  } else {
    message("no row index: ", basename(file), target_row_index)
  }
}

max_len <- max(sapply(results, length))
results_padded <- lapply(results, function(x) c(x, rep(NA, max_len - length(x))))
data <- as.data.frame(do.call(rbind, results_padded), stringsAsFactors = FALSE)
data <- data %>%
  select(V3) %>%
  mutate(id=1:84)
#write.csv(data, "data.csv")
ts_data <- ts(as.numeric(data$V3), start=c(2012, 01), frequency=12)
train <- window(ts_data, end=c(2017, 12))
test <- window(ts_data, start=c(2018, 01))

# ets
ets_model <- ets(train)
ets_model
ets_predict <- forecast(ets_model, h=length(test))
ets_rmse <- sqrt(mean((ets_predict$mean-test)^2))
ets_mape <- mean(abs((ets_predict$mean-test))/test)*100
ets_rmse
ets_mape

# arima
sarima <- auto.arima(train, seasonal=TRUE)
sarima_predict <- forecast(sarima, h=length(test))
sarima

sarima_rmse <- sqrt(mean((sarima_predict$mean-test)^2))
sarima_mape <- mean(abs((sarima_predict$mean-test)/test)) * 100
sarima_rmse
sarima_mape


# graph
autoplot(ts_data) +
  labs(title="Influenza Cases from 2012 to 2018",
       x="Year",
       y="Number of cases")
autoplot(ets_predict, PI=FALSE) +
  labs(title="Prediction from ETS(M,A,M)",
       x="Time", y="Number of Cases")
autoplot(sarima_predict, PI=FALSE) +
  labs(title="Prediction from SARIMA(0,1,0)(1,0,0)[12]",
       x="Time", y="Number of Cases")

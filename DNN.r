# Cài đặt và tải các gói cần thiết
if (!require("keras")) install.packages("keras")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readxl")) install.packages("readxl")
if (!require("plotly")) install.packages("plotly")
if (!require("lubridate")) install.packages("lubridate")
if (!require("quantmod")) install.packages("quantmod")

library(keras)
library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)
library(quantmod)

# Bước 1: Đọc dữ liệu từ file Excel
data <- read_excel("/Users/hoangnguyenbaotram/Downloads/merged.xlsx")

# Bước 2: Chuẩn bị dữ liệu
data <- select(data, Date, open, high, low, close, volume, Symbol)
data$Date <- as.Date(data$Date)

# Thiết lập mã cổ phiếu được chọn để phân tích
selected_symbol <- "AAPL"

if (selected_symbol %in% data$Symbol) {
  cat("Processing Symbol:", selected_symbol, "\n")
  symbol_data <- filter(data, Symbol == selected_symbol)
  
  # Tách dữ liệu thành dữ liệu huấn luyện
  training_data <- symbol_data %>% filter(Date <= as.Date("2023-12-29"))
  
  # Thêm các đặc trưng dựa trên ngày
  training_data <- training_data %>%
    mutate(
      Year = year(Date),
      Month = month(Date),
      Day = day(Date),
      DayOfWeek = wday(Date)
    )
  
  # Chọn các đặc trưng cần thiết
  features <- c("Year", "Month", "Day", "DayOfWeek", "open", "high", "low", "volume")
  response <- "close"
  
  if (nrow(training_data) == 0 || !all(features %in% names(training_data))) {
    cat("Training data is empty or features are not selected properly.\n")
    return()
  }
  
  # Chuẩn bị dữ liệu huấn luyện
  X_train <- as.matrix(training_data %>% select(all_of(features)))
  y_train <- as.matrix(training_data[[response]])
  
  # Chuẩn hóa dữ liệu
  X_scaled <- scale(X_train)
  scaled_center <- attr(X_scaled, "scaled:center")
  scaled_scale <- attr(X_scaled, "scaled:scale")
  
  # Huấn luyện mô hình
  set.seed(42)
  indices <- sample(1:nrow(X_scaled), size = 0.8 * nrow(X_scaled))
  X_train_train <- X_scaled[indices, ]
  X_train_test <- X_scaled[-indices, ]
  y_train_train <- y_train[indices]
  y_train_test <- y_train[-indices]
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = 'relu', input_shape = dim(X_train_train)[2]) %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = 'rmsprop',
    loss = 'mse',
    metrics = c('mean_absolute_error')
  )
  
  history <- model %>% fit(
    X_train_train,
    y_train_train,
    epochs = 100,
    validation_split = 0.2,
    verbose = 0
  )
  
  # Đánh giá mô hình với tập kiểm tra
  evaluation <- model %>% evaluate(X_train_test, y_train_test, verbose = 0)
  cat("Độ lỗi trung bình tuyệt đối trên tập kiểm tra:", evaluation[2], "\n\n")
  
  # Lấy dữ liệu giá thực tế từ Yahoo Finance
  getSymbols(selected_symbol, src = "yahoo", from = "2024-01-01", to = "2024-05-15")
  actual_data_to_may <- fortify.zoo(AAPL)
  actual_data_to_may <- actual_data_to_may %>% mutate(Date = index(AAPL)) %>%
    select(Date, AAPL.Close) %>%
    rename(close = AAPL.Close)
  
  # Dự đoán giá từ ngày 1/1/2024 đến ngày 15/5/2024
  prediction_dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
  future_data <- data.frame(Date = prediction_dates) %>%
    mutate(
      Year = year(Date),
      Month = month(Date),
      Day = day(Date),
      DayOfWeek = wday(Date),
      open = mean(training_data$open, na.rm = TRUE),  # Using mean of training data or another method
      high = mean(training_data$high, na.rm = TRUE),
      low = mean(training_data$low, na.rm = TRUE),
      volume = mean(training_data$volume, na.rm = TRUE)
    )
  
  if (!all(features %in% names(future_data))) {
    cat("Future data is missing some of the required features.\n")
    return()
  }
  
  X_future <- as.matrix(future_data %>% select(all_of(features)))
  X_future_scaled <- scale(X_future, center = scaled_center, scale = scaled_scale)
  predictions <- model %>% predict(X_future_scaled)
  prediction_results <- data.frame(Date = future_data$Date, Predicted = as.vector(predictions))
  
  # Biểu đồ lịch sử huấn luyện
  history_df <- as.data.frame(history$metrics) %>%
    mutate(epoch = 1:nrow(.)) %>%
    pivot_longer(-epoch, names_to = "metric", values_to = "value")
  
  p1 <- ggplot(history_df %>% filter(metric %in% c("loss", "val_loss")),
               aes(x = epoch, y = value, color = metric)) +
    geom_line() +
    labs(title = "Training and Validation Loss", x = "Epoch", y = "Loss", color = "Metric") +
    theme_minimal()
  
  p2 <- ggplot(history_df %>% filter(metric %in% c("mean_absolute_error", "val_mean_absolute_error")),
               aes(x = epoch, y = value, color = metric)) +
    geom_line() +
    labs(title = "Training and Validation MAE", x = "Epoch", y = "MAE", color = "Metric") +
    theme_minimal()
  
  # Biểu đồ dự đoán giá cổ phiếu cho năm 2024
  p3 <- ggplot(prediction_results, aes(x = Date, y = Predicted)) +
    geom_line(color = 'blue') +
    labs(title = paste("Stock price forecast for", selected_symbol, "through December 31, 2024"),
         x = "Date", y = "Predicted Price") +
    theme_minimal()
  
  # Biểu đồ đánh giá hiệu suất mô hình
  performance_data <- bind_rows(
    data.frame(Date = actual_data_to_may$Date, Value = actual_data_to_may$close, Type = "Actual"),
    data.frame(Date = prediction_results$Date, Value = prediction_results$Predicted, Type = "Predicted")
  )
  
  p4 <- ggplot(performance_data, aes(x = Date, y = Value, color = Type)) +
    geom_line() +
    labs(title = paste("Forecast and Actual Price for", selected_symbol, "until May 15, 2024"),
         x = "Date", y = "Price", color = "Type") +
    theme_minimal()
  
  # Hiển thị các biểu đồ
  print(ggplotly(p1))
  print(ggplotly(p2))
  print(ggplotly(p3))
  print(ggplotly(p4))
  
} else {
  cat("The selected symbol", selected_symbol, "is not in the dataset.\n")
}


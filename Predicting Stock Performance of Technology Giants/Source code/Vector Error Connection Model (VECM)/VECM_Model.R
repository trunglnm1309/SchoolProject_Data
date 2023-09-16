#Import dữ liệu
df<-read.csv(file.choose(), header = TRUE)

#Chuyển đổi các biến thành dạng time-series
gold <- ts(df$Gold, frequency = 365, start = c(2020, 1))
oil <- ts(df$CrudeOil, frequency = 365, start = c(2020, 1))

#Chia train set & test set
train_start <- c(2020, 1)
train_end <- c(2022, 17)
test_start <- c(2022, 18)
test_end <- c(2022, 47)
train_gold <- window(gold, start = train_start, end = train_end)
test_gold <- window(gold, start = test_start, end = test_end)
train_oil <- window(oil, start = train_start, end = train_end)
test_oil <- window(oil, start = test_start, end = test_end)

#Thực hiện kiểm định ADF để kiểm tra tính stationary của từng biến
library(tseries)
gold_adf<-adf.test(train_gold)
gold_adf
oil_adf<-adf.test(train_oil)
oil_adf

#Gán train data thành 1 ma trận
train_data <- cbind(train_gold, train_oil)

#Thực hiện kiểm định Johansen để kiểm tra quan hệ cointegration
library(urca)
coint_test <- ca.jo(train_data, type = "trace", ecdet = "trend", K = 2)
summary(coint_test)

#Tạo bậc cho mô hình VECM
library(vars)
model_order <- VARselect(train_data, lag.max = 10, type = "const")
model_order 
 
#Tạo mô hình VECM
library(tsDyn)
vecm_model <- VECM(train_data, lag = model_order$selection[1], r = 1, include = "const", estim = "2OLS")
summary(vecm_model)

#Dự đoán biến trong 30 ngày tiếp theo trên tập train
forecast <- predict(vecm_model, n.ahead = 30)

#Biểu diễn dự đoán lên biểu đồ
library(ggplot2)
#Tạo dataframe
predict_df <- data.frame(Date = time(forecast ), 
                         Gold = forecast [,1], 
                         Oil = forecast [,2])
#Tạo biểu đồ
plot_vecm<-ggplot(data = predict_df, aes(x = Date)) + 
       geom_line(aes(y = Gold, color = "Gold")) + 
       geom_line(aes(y = Oil, color = "Oil")) + 
       scale_color_manual(name = "Asset", values = c("Gold" = "red", "Oil" = "blue")) + 
       xlab("Time") + 
       ylab("Price") + 
       ggtitle("Gold and Oil Prices Prediction")
plot_vecm

#Đánh giá hiệu suất
library(forecast)
accuracy_test_gold <- accuracy(predicted_values_gold, test_gold)
accuracy_test_oil <- accuracy(predicted_values_oil, test_oil)

dff <- data.frame(Date = index(test_gold),
                  Predicted_Gold = predicted_values_gold,
                  Predicted_Oil = predicted_values_oil,
                  Actual_Gold = test_gold,
                  Actual_Oil = test_oil)
plot_test <- ggplot(dff, aes(x = Date)) +
       geom_line(aes(y = Predicted_Gold, color = "Predicted Gold")) +
       geom_line(aes(y = Predicted_Oil, color = "Predicted Oil")) +
       geom_line(aes(y = Actual_Gold, color = "Actual Gold")) +
       geom_line(aes(y = Actual_Oil, color = "Actual Oil")) +
       labs(x = "Date", y = "Price", color = "Series") +
       ggtitle("Comparison of Predicted and Actual Prices") +
       theme_bw()
plot_test

accuracy_test_gold 
accuracy_test_oil 

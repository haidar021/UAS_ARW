

# Load library
library(quantmod)   # to load the dataset we will be using
library(ggplot2)    # to create charts
library(gridExtra)  #to arrange multiple plots into one
library(forecast)   # to forecast our models
library(tseries)    # to perform multiple testing of our models later
library(zoo)        # to convert dataset format
library(kableExtra) # to create tidy tables
library(dplyr)      # to clean the dataset


setwd("C:/Users/LENOVO/OneDrive/Dokumen/analisis runtun waktu")
data <- read_csv("data_cctv.csv")
data$Tanggal <- as.Date(data$`Tanggal Pengamatan`, format="%m/%d/%Y")
test1 <- xts(data$`Jumlah Mobil`, order.by = data$Tanggal)
colnames(test1) <- "JumlahMobil"  

# 2. PROSES DATA
a <- test1$JumlahMobil  
b <- dailyReturn(a)

test3 = merge(a, b, all=FALSE) |>
  fortify.zoo() |>
  rename(close = "JumlahMobil") |>
  rename(dr = "daily.returns") |>
  rename(date = "Index")

# 3. PLOT CLOSING DAN RETURN
fig1 = test3 |>
  ggplot(aes(x = date, y = close)) +
  geom_line() +
  labs(x = "Time", y = "Jumlah Mobil") +
  ggtitle("Nonstationary")

fig2 = test3 |>
  ggplot(aes(x = date, y = dr)) +
  geom_line() +
  labs(x = "Time", y = "Daily Return") +
  ggtitle("Stationary")

grid.arrange(fig1, fig2)

# 4. UJI STASIONERITAS
adf.test(test3$close)
ggAcf(test3$close, type = "correlation") + 
  ggtitle("ACF Plot : Nonstationary") + 
  theme_classic()

# 5. MODEL NON-STASIONER
ns_ar = auto.arima(test3$close, max.d = 0, max.q = 0, allowdrift = TRUE)
ns_ma = auto.arima(test3$close, max.d = 0, max.p = 0, allowdrift = TRUE)
ns_arma = auto.arima(test3$close, max.d = 0, allowdrift = TRUE)
ns_arima = auto.arima(test3$close, allowdrift = TRUE)

# 6. RESIDUAL DAN ACF
res_ns_ar = resid(ns_ar)
res_ns_ma = resid(ns_ma)
res_ns_arma = resid(ns_arma)
res_ns_arima = resid(ns_arima)

fig3 = ggAcf(res_ns_ar, type = "partial")
fig4 = ggAcf(res_ns_ma, type = "partial")
fig5 = ggAcf(res_ns_arma, type = "partial")
fig6 = ggAcf(res_ns_arima, type = "partial")
grid.arrange(fig3, fig4, fig5, fig6, nrow = 2, ncol = 2)

# 7. MODEL STASIONER (RETURN)
adf.test(test3$dr)
ggAcf(test3$dr, type = "correlation") +
  ggtitle("ACF Plot: Stationary") + theme_classic()

s_ar = auto.arima(test3$dr, max.d = 0, max.q = 0, allowdrift = TRUE)
s_ma = auto.arima(test3$dr, max.d = 0, max.p = 0, allowdrift = TRUE)
s_arma = auto.arima(test3$dr, max.d = 0, allowdrift = TRUE)
s_arima = auto.arima(test3$dr, allowdrift = TRUE)

res_s_ar = resid(s_ar)
res_s_ma = resid(s_ma)
res_s_arma = resid(s_arma)
res_s_arima = resid(s_arima)

fig7 = ggAcf(res_s_ar, type = "partial")
fig8 = ggAcf(res_s_ma, type = "partial")
fig9 = ggAcf(res_s_arma, type = "partial")
fig10 = ggAcf(res_s_arima, type = "partial")
grid.arrange(fig7, fig8, fig9, fig10, nrow = 2, ncol = 2)

# 8. UJI WHITE NOISE
Box.test(res_ns_ar, type = "Ljung-Box", lag = 1)
Box.test(res_ns_ma, type = "Ljung-Box", lag = 1)
Box.test(res_ns_arma, type = "Ljung-Box", lag = 1)
Box.test(res_ns_arima, type = "Ljung-Box", lag = 1)

# 9. FORECAST NON-STASIONER
fc_ns_ar = forecast(ns_ar, h = 360, level = 80)
fc_ns_ma = forecast(ns_ma, h = 360, level = 80)
fc_ns_arma = forecast(ns_arma, h = 360, level = 80)
fc_ns_arima = forecast(ns_arima, h = 360, level = 80)

fig11 = autoplot(fc_ns_ar)
fig12 = autoplot(fc_ns_ma)
fig13 = autoplot(fc_ns_arma)
fig14 = autoplot(fc_ns_arima)
grid.arrange(fig11, fig12, fig13, fig14, nrow = 4, ncol = 1)

# 10. UJI WHITE NOISE STASIONER
Box.test(res_s_ar, type = "Ljung-Box", lag = 1)
Box.test(res_s_ma, type = "Ljung-Box", lag = 1)
Box.test(res_s_arma, type = "Ljung-Box", lag = 1)
Box.test(res_s_arima, type = "Ljung-Box", lag = 1)

# 11. FORECAST STASIONER
fc_s_ar = forecast(s_ar, h = 360, level = 80)
fc_s_ma = forecast(s_ma, h = 360, level = 80)
fc_s_arma = forecast(s_arma, h = 360, level = 80)
fc_s_arima = forecast(s_arima, h = 360, level = 80)

fig15 = autoplot(fc_s_ar)
fig16 = autoplot(fc_s_ma)
fig17 = autoplot(fc_s_arma)
fig18 = autoplot(fc_s_arima)
grid.arrange(fig15, fig16, fig17, fig18, nrow = 4, ncol = 1)


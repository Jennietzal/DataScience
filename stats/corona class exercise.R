library(readr)

corona_positives <- read_csv("C:/Users/Xnes/DataScience/data/corona_positives.csv", 
                             col_types = cols(test_date = col_date(format = "%Y-%m-%d"), 
                                              positives = col_integer()))
View(corona_positives)

dts_corona_pos <- ts(corona_positives$positives, start= min(corona_positives$test_date),end=max(corona_positives$test_date), frequency=4)


stationary_dts_corona_pos <- decompose(dts_corona_pos)

plot(stationary_dts_corona_pos)

acf(dts_corona_pos)

pacf(dts_corona_pos)

dts_arima <- arima(dts_corona_pos, order=c(2,1,1))
dts_arima

BIC(dts_arima)

library(forecast)
dts_fit <- forecast(dts_arima)
dts_fit

plot(dts_fit)

dts_autoarima <- auto.arima(dts_corona_pos)
dts_autoarima

dts_autoforecast <- forecast(dts_autoarima)
dts_autoforecast

plot(dts_autoforecast)

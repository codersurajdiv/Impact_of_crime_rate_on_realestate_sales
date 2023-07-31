library(forecast)

data <- read.csv("Final dataset.csv", stringsAsFactors = FALSE)
data$date = as.Date(data$Month, format="%m%d%Y")

#Crate ts datasets for each variable
ts_violence <- ts(data$Violence.Against.the.Person, start=c(2008,1), end = c(2016,12), frequency = 12)
ts_theft <- ts(data$Theft.and.Handling, start=c(2008,1), end = c(2016,12), frequency = 12)
ts_robbery <- ts(data$Robbery, start=c(2008,1), end = c(2016,12), frequency = 12)
ts_drugs <- ts(data$Drugs, start=c(2008,1), end = c(2016,12), frequency = 12)
ts_criminaldamage<- ts(data$Criminal.Damage, start=c(2008,1), end = c(2016,12), frequency = 12)
ts_burglary <- ts(data$Burglary, start=c(2008,1), end = c(2016,12), frequency = 12)

#Plot the ts
plot(ts_violence, xlab="Year", ylab="Violence Against Person Count")
plot(ts_theft, xlab="Year", ylab="Theft Count")
plot(ts_robbery, xlab="Year", ylab="Robbery Count")
plot(ts_drugs, xlab="Year", ylab="Drugs Offences Count")
plot(ts_criminaldamage, xlab="Year", ylab=" Criminal Damage Count")
plot(ts_burglary, xlab="Year", ylab="Burglary Count")

#Build arima model
model1 <- auto.arima(ts_violence)
model2 <- auto.arima(ts_theft)
model3 <- auto.arima(ts_robbery)
model4 <- auto.arima(ts_drugs)
model5 <- auto.arima(ts_criminaldamage)
model6 <- auto.arima(ts_burglary)

#View model characteristics
model1
model2
model3
model4
model5
model6

#Forecast future values for the next 5 years
forecast_violence <- forecast(model1, level = c(95), h=5*12)
forecast_theft <- forecast(model2, level = c(95), h=5*12)
forecast_robbery <- forecast(model3, level = c(95), h=5*12)
forecast_drugs <- forecast(model4, level = c(95), h=5*12)
forecast_criminaldamage <- forecast(model5, level = c(95), h=5*12)
forecast_burglary <- forecast(model6, level = c(95), h=5*12)

#view forecasted values
forecast_violence
forecast_theft
forecast_robbery
forecast_drugs
forecast_criminaldamage
forecast_burglary

#Plot forecast
plot(forecast_violence)
plot(forecast_theft)
plot(forecast_robbery)
plot(forecast_drugs)
plot(forecast_criminaldamage)
plot(forecast_burglary)

#time series of house sales
ts_housessold <- ts(data$Number.of.Houses.Sold, start=c(2008,1), end = c(2016,12), frequency = 12)
plot(ts_housessold)

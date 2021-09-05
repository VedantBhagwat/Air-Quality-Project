library(zoo)
library(tidyr)

library(astsa)
library(tseries) # for ADF test
library(forecast) # auto.arima

summary(NO2_clone)

library(xts)
library(forecast)
library(TSstudio)
library(timetk)

# time_index <- seq(from = as.POSIXct("2019-01-01 00:00"), 
                  # to = as.POSIXct("2020-06-26 23:00"), by = "hour")
# set.seed(1)
# value <- rnorm(n = length(time_index))
# nrow(time_index)
# eventdata <- xts(value, order.by = time_index)
# ets(eventdata)
# autoplot(eventdata)

# NO2_clone <- NO2_clone[-c(13033 : 13048),]

# NO2_ts <- xts(NO2_clone$Rathmines, order.by = time_index)

# Daily
NO2_ts <- ts(NO2_clone$Rathmines)
ts_info(NO2_ts)

# NO2_ts <- zoo(
#   x         = NO2_clone$Rathmines,
#   order.by  = NO2_clone$Date,
#   frequency = 24*365
# )
# Warning message:
#   In zoo(x = NO2_clone$Rathmines, order.by = NO2_clone$Date) :
#   some methods for “zoo” objects do not work if the index entries in ‘order.by’ are not unique

anyDuplicated(NO2_clone$Date)
sum(duplicated(NO2_clone$Date))
# NO2_clone <- NO2_clone[-8762,]

# NO2_ts <- ts(NO2_ts)
autoplot(NO2_ts , main="Hourly data of Rathmines")

NO2 %>% 
  plot_time_series(as.Date(Date), Rathmines, 
                   .interactive = interactive,
                   .plotly_slider = TRUE)
summary(NO2_ts)


# Week 3
acf2(NO2_ts)

# Trend and Seasonality
NO2.add = decompose(NO2_ts, "additive")
autoplot(NO2.add)
NO2.mul = decompose(NO2_ts, "multiplicative")
autoplot(NO2.mul)


# Week 4
# Movin Average
library(forecast)
plot(NO2_ts)
sm <- ma(NO2_ts, order = 24*30)
lines(sm, col = "red")

# seasonal() extrae la componente estacional.
# trendcycle() componente tendencia-ciclo.
# remainder() compoenente "restante".
# seasadj() ajusta componente estacional.
fit <- stl(NO2_ts, s.window=7)
autoplot(NO2_ts, series="Data") +
  autolayer(seasadj(fit), series="Adjusted Seasonality")

# Season plot
ggseasonplot(window(NO2_ts), year.labels=TRUE) +
  ggtitle("Total Income Tourism") + ylab("??? x 1,000")

## DECOMPOSE

fit <- stl(NO2_ts, s.window=5, robust=TRUE)
autoplot(fit) +
  ggtitle("STL Decomposition NO2_ts")

fit <- stl(NO2_ts, s.window="periodic", robust=TRUE)
autoplot(fit) +
  ggtitle("STL Decomposition NO2_ts")

stl(NO2_ts,s.window=5)
stl(NO2_ts, t.window=15, s.window="periodic", robust=TRUE)

# DECOMPOSITION STL
NO2_ts %>% mstl() %>% autoplot()   ## Multiple Seasonal Trend Loess

# PREDICTION
fit <- stl(NO2_ts, t.window=13, s.window="periodic")
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Seasonal adjusted data ETS Predictions")

# Prediction with forecast
fit %>% forecast(method='naive') %>%
  autoplot() + ylab("New orders index") +
  xlab("Year")

# sftl
NO2_ts %>% stlf(method='naive') %>%
  autoplot() + ylab("New orders index") +
  xlab("Year")


# SES
library(fpp2)
?window
fc <- ses(NO2_ts, h = 24*30)

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("NO2 (ppm)") +
  xlab("year")


## TREND 
window(NO2_ts) %>%
  holt(h=5, PI=FALSE) %>%
  autoplot()

window(NO2_ts) %>%
  holt(damped=TRUE, h=5, PI=FALSE) %>%
  autoplot()


# Fit three models

# NO2_ts2 <- window(NO2_ts, start=as.Date("2019-01-01 00:00"), end=as.Date("2020-06-26 23:00"))
NO2_ts2 <- window(NO2_ts)
fit1 <- ses(NO2_ts2)
# fit2 <- holt(NO2_ts2)
# fit3 <- holt(NO2_ts2, damped = TRUE)
# 
# # Compare models (*training RMSE*)
accuracy(fit1, NO2_ts2)
# accuracy(fit2, NO2_ts2)
# accuracy(fit3, NO2_ts2)
# 
# autoplot(NO2_ts2) +
#   autolayer(fit1, series="SES method", PI=FALSE) +
#   autolayer(fit2, series="Holt's method", PI=FALSE) +
#   autolayer(fit3, series="Damped Holt's method", PI=FALSE) +
#   ggtitle("NO2_ts Prediction method") + xlab("Year") +
#   ylab("NO2_ts") +
#   guides(colour=guide_legend(title="Forecast"))



## METHODS HOLT WINTERS
NO2_ts2 <- window(NO2_ts)
fit1 <- hw(NO2_ts2,seasonal="additive")
fit2 <- hw(NO2_ts2,seasonal="multiplicative")
tmp <- cbind(Data=NO2_ts2,
             "Predictions HW additive" = fit1[["mean"]],
             "Predictions HW multiplicative" = fit2[["mean"]]
)
autoplot(tmp) + xlab("Year") +
  ylab("Hotel Stays (millions)") +
  scale_color_manual(name="", 
                     values=c('#000000','#1b9e77','#d95f02'),
                     breaks=c("Data","Predictions HW additive",
                              "Predictions HW multiplicative"))

# Stimates Components
addstates <- fit1$model$states[,1:3]
# multstates <- fit2$model$states[,1:3]
colnames(addstates)<-colnames(multstates)<-
  c("level","slope","seasonality")
p1 <- autoplot(addstates, facets=TRUE) +
  xlab("year") + ylab("") +
  ggtitle("Additives")
p2 <- autoplot(multstates, facets=TRUE) +
  xlab("year") + ylab("") +
  ggtitle("Multiplicatives")
gridExtra::grid.arrange(p1,p2,ncol=2)
p1


## HW damped
fc <- hw(subset(NO2_ts,end=length(NO2_ts)-(24*15)),
         damped = TRUE, seasonal="multiplicative", h=24*15)
autoplot(NO2_ts) +
  autolayer(fc, series="HW multi damped", PI=FALSE)+
  guides(colour=guide_legend(title="Daily forecasts"))





# ?mstl   ## periodicity
# mstl(NO2_ts)
# autoplot(mstl(NO2_ts))
# autoplot(stlf(NO2_ts)  )
# ?ets  ## exponential smoothing model auto
# ets(NO2_ts)   ## auto.arima


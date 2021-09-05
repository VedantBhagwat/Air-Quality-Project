library(zoo)
library(astsa)
library(ggplot2)
library(forecast)
library(TSstudio)
library(timetk)
library(xts)
library(stats)

df<- NULL
df <- NO2_clone
View(df)
summary(df)

# Convert hourly data in daily data
# stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
df$Date <- as.Date(df$Date, "%m/%d/%Y")
ag <- aggregate(UCC ~ Date, df, mean)

# Aggregate all stations by daily mean
ag2 <- aggregate(. ~ Date, df, mean) 
# write.csv(ag2,"E:\\CIT\\Project\\AQ data\\Export\\Daily NO2 Data.csv", row.names = FALSE)
summary(ag2)

summary(ag)
str(ag)
NO2_ts_daily <- ts(ag$UCC, start = c(2019,1), frequency = 365) # start = c(2019,1), 
ts_info(NO2_ts_daily)

# inds <- seq(as.Date("2019-01-01"), as.Date("2020-06-27"), by = "day")
# NO2_ts_daily <- ts(ag$UCC, start = c(2019, as.numeric(format(inds[1], "%j"))),frequency = 365)
# ts_info(NO2_ts_daily)

# ## use auto.arima to choose ARIMA terms
# fit <- auto.arima(NO2_ts_daily)
# ## forecast for next 60 time points
# fore <- forecast(fit, h = 60)
# ## plot it
# plot(fore)

# NO2_ts_daily <- zoo(
#   x         = ag$UCC,
#   order.by  = ag$Date,
#   frequency = 365
# )
# ts_info(NO2_ts_daily)

NO2_ts_daily <- ag$UCC

dfts <- as.ts(xts(ag$UCC, order.by=ag$Date))
NO2_ts_daily <- ts(dfts)

ts_info(NO2_ts_daily)
ts_plot(NO2_ts_daily)

plot.ts(NO2_ts_daily)
autoplot(NO2_ts_daily)

NO2_clone %>% 
  plot_time_series(as.Date(Date), UCC, 
                   .interactive = interactive,
                   .plotly_slider = TRUE)

sum(is.na(NO2_ts_daily))
NO2_ts_daily <- round(NO2_ts_daily,2)
       # median(NO2_clone[,i], na.rm = TRUE), 2)

# Week 3
acf2(NO2_ts_daily)

# Trend and Seasonality
NO2.add = decompose(NO2_ts_daily, "additive")
autoplot(NO2.add)
NO2.mul = decompose(NO2_ts_daily, "multiplicative")
autoplot(NO2.mul)

# Week 4
# Movin Average

plot(NO2_ts_daily)
sm <- ma(NO2_ts_daily, order = 12)
lines(sm, col = "red")

# seasonal() extrae la componente estacional.
# trendcycle() componente tendencia-ciclo.
# remainder() compoenente "restante".
# seasadj() ajusta componente estacional.
fit <- stl(NO2_ts_daily, s.window=7)
autoplot(NO2_ts_daily, series="Data") +
  autolayer(seasadj(fit), series="Adjusted Seasonality")

# Season plot
ggseasonplot(window(NO2_ts_daily), year.labels=TRUE) +
  ggtitle("Total Income Tourism") + ylab("??? x 1,000")


## DECOMPOSE
fit <- stl(NO2_ts_daily, s.window=5, robust=TRUE)
autoplot(fit) +
  ggtitle("STL Decomposition NO2_ts_daily")

fit <- stl(NO2_ts_daily, s.window="periodic", robust=TRUE)
autoplot(fit) +
  ggtitle("STL Decomposition NO2_ts_daily")

stl(NO2_ts_daily,s.window=5)
stl(NO2_ts_daily, t.window=15, s.window="periodic", robust=TRUE)

# DECOMPOSITION STL
NO2_ts_daily %>% mstl() %>% autoplot()   ## Multiple Seasonal Trend Loess

# PREDICTION
fit <- stl(NO2_ts_daily, t.window=13, s.window="periodic")
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Seasonal adjusted data ETS Predictions")

# Prediction with forecast
fit %>% forecast(method='naive') %>%
  autoplot() + ylab("New orders index") +
  xlab("Year")

# sftl
NO2_ts_daily %>% stlf(method='naive') %>%
  autoplot() + ylab("New orders index") +
  xlab("Year")


# SES
library(fpp2)
?window
fc <- ses(NO2_ts_daily, h = 30)

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("NO2 (ppm)") +
  xlab("year")


## TREND 
window(NO2_ts_daily) %>%
  holt(h=30, PI=FALSE) %>%
  autoplot()

window(NO2_ts_daily) %>%
  holt(damped=TRUE, h=30, PI=FALSE) %>%
  autoplot()


##########  Fit three models (Errors)
# Error in window.default(x, ...) : 'start' cannot be after 'end'
# In addition: Warning message:
#   In window.default(x, ...) : 'start' value not changed
# NO2_ts_daily2 <- window(NO2_ts_daily, start=as.Date("2019-01-01 00:00"), end=as.Date("2020-06-26 23:00"))
NO2_ts_daily2 <- window(NO2_ts_daily)
fit1 <- ses(NO2_ts_daily2)
fit2 <- holt(NO2_ts_daily2)
fit3 <- holt(NO2_ts_daily2, damped = TRUE)
# 
# # Compare models (*training RMSE*)
accuracy(fit1, NO2_ts_daily2)
accuracy(fit2, NO2_ts_daily2)
accuracy(fit3, NO2_ts_daily2)
# 
autoplot(NO2_ts_daily2) +
  autolayer(fit1, series="SES method", PI=FALSE) +
  autolayer(fit2, series="Holt's method", PI=FALSE) +
  autolayer(fit3, series="Damped Holt's method", PI=FALSE) +
  ggtitle("NO2_ts_daily Prediction method") + xlab("Year") +
  ylab("NO2_ts_daily") +
  guides(colour=guide_legend(title="Forecast"))



## METHODS HOLT WINTERS (It works)
NO2_ts_daily2 <- window(NO2_ts_daily)
fit1 <- hw(NO2_ts_daily2,seasonal="additive")
fit2 <- hw(NO2_ts_daily2,seasonal="multiplicative")
tmp <- cbind(Data=NO2_ts_daily2,
             "Predictions HW additive" = fit1[["mean"]],
             "Predictions HW multiplicative" = fit2[["mean"]]
)
autoplot(tmp) + xlab("Year") +
  ylab("Hotel Stays (millions)") +
  scale_color_manual(name="", 
                     values=c('#000000','#1b9e77','#d95f02'),
                     breaks=c("Data","Predictions HW additive",
                              "Predictions HW multiplicative"))

# Stimates Components (works)
addstates <- fit1$model$states[,1:3]
multstates <- fit2$model$states[,1:3]
colnames(addstates)<-colnames(multstates)<-
  c("level","slope","seasonality")
p1 <- autoplot(addstates, facets=TRUE) +
  xlab("year") + ylab("") +
  ggtitle("Additives")
p2 <- autoplot(multstates, facets=TRUE) +
  xlab("year") + ylab("") +
  ggtitle("Multiplicatives")
gridExtra::grid.arrange(p1,p2,ncol=2)


## HW damped
fc <- hw(subset(NO2_ts_daily,end=length(NO2_ts_daily)-(30)),
         damped = TRUE, seasonal="multiplicative", h=30)
autoplot(NO2_ts_daily) +
  autolayer(fc, series="HW multi damped", PI=FALSE)+
  guides(colour=guide_legend(title="Daily forecasts"))


# ETS WITH R

autoplot(NO2_ts_daily) + xlab("Year") + ylab("NO2")
ets(NO2_ts_daily)
ets(NO2_ts_daily, model = "AAA", damped = FALSE)
NO2_ts_daily %>% ets() %>% autoplot()
NO2_ts_daily %>% ets() %>% forecast() %>% autoplot()
NO2_ts_daily %>% ets() %>% accuracy()
NO2_ts_daily %>% ets(model = "AAA", damped = FALSE) %>% accuracy()


# train <- window(NO2_ts_daily, end = as.Date("2020-06-26")) # data 'calentamiento'
# test <- window(NO2_ts_daily, start = 2019) # model adjust 'train'
# fit1 <- ets(train)
# fit2 <- ets(test, model = fit1, use.initial.values=TRUE)
# 
# accuracy(fit2)
# accuracy(forecast(fit1, 10), test)

##### Week 5
par(mfrow=c(1,3))

NO2_ts_daily.fcast1 <- hw(NO2_ts_daily,  seasonal="multiplicative",  h=48)
summary(NO2_ts_daily.fcast1)
plot(NO2_ts_daily.fcast1)
accuracy(NO2_ts_daily.fcast1)

NO2_ts_daily.fcast2 <- hw(NO2_ts_daily,  seasonal="additive",  h=48)
summary(NO2_ts_daily.fcast2)
plot(NO2_ts_daily.fcast2)
accuracy(NO2_ts_daily.fcast2)


NO2_ts_daily.fcast3 <- hw(NO2_ts_daily,  seasonal="additive",  damped = TRUE, h=48)
summary(NO2_ts_daily.fcast3)
plot(NO2_ts_daily.fcast3)
accuracy(NO2_ts_daily.fcast3)


# regression method
fit <- tslm(NO2_ts_daily ~ trend + season)
summary(fit)
plot(forecast(fit, h = 30))



fit1 <- ses(NO2_ts_daily[1:443], h = 100) # 200
accuracy(fit1)
plot(fit1)
lines(NO2_ts_daily[1:543]) 
accuracy(fit1, NO2_ts_daily[201:300])

fit2 <- holt(NO2_ts_daily[1:443], h = 100) # 200
accuracy(fit2)
plot(fit2)
lines(NO2_ts_daily[1:543])
accuracy(fit2, NO2_ts_daily[201:300])

# fit3 <- hw(NO2_ts_daily[1:200], h = 100,  seasonal="multiplicative",  exponential =TRUE)
# plot(fit3)
# accuracy(fit3)


#Regression 
fitR <- tslm(NO2_ts_daily ~ trend + season)
plot(forecast(fitR, h = 100))
lines(NO2_ts_daily[1:300]) 



# Week 7
# Data Transformation
# Trend by Diff and Regression

dprec = diff(NO2_ts_daily)     # 1st diff
d2prec = diff(NO2_ts_daily, diff=2) # 2nd diff

#
dev.off()
par(mfrow=c(2,3))
#autoplot(NO2_ts_daily)
#autoplot(dprec)
#autoplot(d2prec)
plot(NO2_ts_daily, main="NO2 m2")
plot(dprec, main="D(NO2)")
plot(d2prec, main="D2(NO2)") 
# ACF comparison
acf(NO2_ts_daily, main="NO2m2")
acf(dprec, main="D(NO2m2)")
acf(d2prec, main="D2(NO2m2)")
# Decompose and Regression using Trend and Seasonality
dec = decompose(NO2_ts_daily)
autoplot(dec)

ddec = decompose(dprec)
autoplot(ddec)

d2dec = decompose(d2prec)
autoplot(d2dec)

ff = lm(d2prec ~ d2dec$trend + d2dec$seasonal )
plot(d2prec)
abline(ff, col=2)

# # Regression line to the TS 
# fit <- tslm(NO2_ts_daily ~ trend)
# #fit2 <- tslm(NO2_ts_daily ~ trend + season )
# summary(fit)
# fr = fit$residuals
# plot(fr, main="Residuals")
# plot(fit)
# plot.ts(NO2_ts_daily)
# abline(fit, col=2)


# Only Check - fit regression to 1st diff
dfit <- tslm(dprec ~ trend + season)
dfit2 <- tslm(dprec ~ trend )
summary(dfit)
dfr = dfit$residuals
plot(dfr, main="D(residuals)")

# Only Check - fit regression to 2nd diff
d2fit <- tslm(d2prec ~ trend + season)
d2fit2 <- tslm(d2prec ~ trend )
summary(d2fit)
d2fr = d2fit$residuals
plot(d2fr, main="D2(residuals)", type="p")
lines(d2fr, col=2)

# Fit Quadratic Regression
fitQ <- tslm(NO2_ts_daily ~ trend + I(trend^2) )

summary(fitQ)
frQ = fitQ$residuals
plot(frQ, main="Quadratic Residuals")

# Fit Cubic Regression
fitC <- tslm(NO2_ts_daily ~ trend + I(trend^2)+ I(trend^3) )

summary(fitC)
frC = fitC$residuals
plot(frC, main="Cubic Residuals")


# ARIMA model
dev.off()
plot(NO2_ts_daily)
acf2(NO2_ts_daily)
fit = arima(NO2_ts_daily, order = c(0L, 1, 1),
      seasonal = list(order = c(0L, 0L, 0L),
                      period = NA),
      xreg = NULL, include.mean = TRUE,
      transform.pars = TRUE,
      fixed = NULL, init = NULL,
      method = c("CSS-ML", "ML", "CSS"),
      SSinit = c("Gardner1980", "Rossignol2011"),
      optim.method = "BFGS",
      optim.control = list(), kappa = 1e6)
fit

adf.test(NO2_ts_daily, alternative="stationary", k=0)
kpss.test(NO2_ts_daily)


############################
####  SEASONALITY  #########
####  ===========  #########

## there are log & diff to transform into Stationary TS
## New problem --> SEASONALITY
# ARIMA(p,q,d)xSARIMA(P,Q,D)s

sarima(xdata, p, d, q, P = 0, D = 0, Q = 0, S = -1,
       details = TRUE, xreg=NULL, Model=TRUE,
       tol = sqrt(.Machine$double.eps),
       no.constant = FALSE)
# p: AR order (must be specified)
# d: difference order (must be specified)
# q: MA order (must be specified)
# P: SAR order; use only for seasonal models
# D: seasonal difference; use only for seasonal models
# Q: SMA order; use only for seasonal models
# S: seasonal period; use only for seasonal models
##
dev.off()
model = sarima(NO2_ts_daily, 2,0,0) ## only 3 param out of 7 
model2 = sarima(NO2_ts_daily, 2,0,0,1,0,0,10) ## still problems
model3 = sarima(NO2_ts_daily, 2,0,0,0,0,1,10) ##  still problems
model4 = sarima(NO2_ts_daily, 1,1,0,0,0,2,12)

library(FinTS) #for function `ArchTest()`
library(rugarch) #for GARCH models
library(rmgarch)
library(quantmod)
library(tseries) # for `adf.test()`
library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(PoEdata) #for PoE4 datasets
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for `kable()`
library(forecast) 
library(dplyr)
library(hrbrthemes)

library(stats)
library(tseries)
library(ggplot2)
library(forecast)
library(TTR)
library(ggthemes)
library(gganimate)
library(magick)
library(astsa)
library(lmtest)
library(TSA)
library(MTS)
library(vars)

par(mar = c(5,4,3,2)+0.1)
par(oma = c(0.5,0.5,0.5,0.5))
par(mfrow=c(1,1))

#loading the data 
data=Ice_area
ice.extent=na.omit((data$extent1))
length(ice.extent)
tail(ice.extent)





#making a ts obejct
ice.series=ts(ice.extent,start=c(2006,1),frequency = 12)
class(ice.series)
plot(ice.series)
autoplot(ts(ice.extent,start=c(2006,1),frequency = 12)) +geom_line(color="blue", size=0.7)+
  ggtitle("Time Series Plot")+xlab("Year")+ylab("ice_extent (in sq km)")+theme(plot.title = element_text(hjust = 0.5))
animated_plot <- ggplot(data, aes(x=date, y=extent1)) +
  geom_line(color="blue4", size=0.7) + geom_point(size=2) + transition_reveal(date)+ theme(plot.title = element_text(hjust = 0.9)) + theme_economist(base_size = 14) + scale_color_economist() +xlab("Year")+ylab("Ice Cover (sq km)")
animation <- animate(animated_plot, nframes=176, renderer=magick_renderer())
animation

#checking if additive/multiplicative 
ets=ets(ice.series)
ets 
#(error,trend,seasonal)
#A- addtive, N- Null, M- Multiplicative
#ANA reflects addtive model
BoxCox.lambda(ice.series)#lamda= 1 (no transformation)
 
#components of ts 
decompose=decompose(ice.series)
plot(decompose)
seasonal=decompose$seasonal
plot(seasonal)
trend=decompose$trend
plot(trend) 

#stationary test 
adf.test(ice.series)
ndiffs(ice.series,test="adf")
nsdiffs(ice.series)
kpss.test(ice.series,null="Level") #not mean stationary 
#kpss.test(ice.series,null="Trend") #deterministic trend

#checking for 1st difference 
df=diff(ice.series)
adf.test(df)
var(ice.series)>var(df)

#test and training data 
training=window(ice.series,end=c(2017,12))
test=window(ice.series,start=c(2018,1),end=c(2020,8))

#checking if training is stationary
d.training=decompose(training)
plot(d.training)
adf.test(training)
ndiffs(training,test="adf")
nsdiffs(training)

#Holtwinters
f = hw(training, seasonal = "additive", h = 32)
autoplot(f)
checkresiduals(f)
holtwinter=HoltWinters(training,beta = F)
holtwinter
forecast1=forecast:::forecast.HoltWinters(holtwinter,h=32)
plot(forecast1)
checkresiduals(holtwinter)
residuals=na.omit(forecast$residuals)

#for Box.test generate residuals by org-fitted - not required 
residuals=training- holtwinter$fitted[,1]
Box.test(residuals,type="Ljung-Box")

#check
autoplot(forecast1)+autolayer(test,series = "Test Data")
#check numerical values also 

#Box Jenkins - SARMA Model
acf=acf(training,lag.max = 36,plot=F)
acf$lag <- acf$lag * 12# transform lag from years to months
plot(acf)
pacf=pacf(training,lag.max = 36,plot=F)
pacf$lag=pacf$lag*12 
plot(pacf)
sarma=auto.arima(training,ic="aic",test="adf",seasonal=T)
sarma
forecast2=forecast(sarma,h=32)
plot(forecast2)
checkresiduals(sarma)
residuals=na.omit(forecast2$residuals)
Box.test(residuals,type="Ljung-Box",fitdf =3,lag=24) #fitdf = p+q
#the lag value is found iteratively, and specify the lag at which autocorrelation is shown 

#check
autoplot(forecast2)+autolayer(test,series = "Test Data")

#AR2 model
ar2=arima(training,order=c(2,0,0))
ar2

#SARIMA - manual parameters
sarima=arima(training, order=c(1L,1L,2L),seasonal=list(order=c(0L,1L,1L),period=12))
class(sarima)
forecast5=predict(sarima,n.ahead=32)
checkresiduals(sarima)

#check
autoplot(forecast5)+autolayer(test,series = "Test Data")

s.training=diff(training,lag=12)
ns.training=diff(s.training)
acf=acf(ns.training,lag.max = 36,plot=F)
acf$lag <- acf$lag * 12# transform lag from years to months
plot(acf) #q=1,Q=1 oR q=2
pacf=pacf(ns.training,lag.max = 36,plot=F)
pacf$lag=pacf$lag*12 
plot(pacf) #P=1,p=2

#SARIMA - manual parameters - 2
sarima2=sarima(training, p = 2, d = 1, q = 1, P = 1, D = 1, Q = 1, S = 12)
sarima2
checkresiduals(sarima2)
sarima2$fit$residuals
forecast6=sarima.for(training,n.ahead=32, p = 2, d = 1, q = 1, P = 1, D = 1, Q = 1, S = 12)
forecast6
forecast6$pred
checkresiduals(sarima$residuals)


#check
autoplot(forecast6$pred)+autolayer(test,series = "Test Data")

#ets 
ets=ets(training)
ets

forecast3=forecast.ets(ets,h=32)
plot(forecast3)

#not required - residuals 
checkresiduals(ets)
residuals=na.omit(forecast3$residuals) #Ljung Box test rejected 
Box.test(residuals,type="Ljung-Box",fitdf =3,lag=24) 

#check
autoplot(forecast3)+autolayer(test,series = "Test Data")

#checking accuracy- use MAPE to check test value
accuracy(forecast1,ice.series) #holtwinters
accuracy(forecast2,ice.series) #auto.sarima
accuracy(forecast5$pred,ice.series) #sarima manual 
accuracy(forecast6$pred,ice.series) #sarima manual 2 
accuracy(forecast3,ice.series) #ets
accuracy(f,ice.series)
#Models 
#Residuals - NA for Holt 
#Autoplot - SARIMA 2
#Accuracy - SARIMA 2

#hence we will forecast using sarima2 model 
fit=sarima(ice.series, p = 2, d = 1, q = 1, P = 1, D = 1, Q = 1, S = 12)
forecast=sarima.for(ice.series,n.ahead=12, p = 2, d = 1, q = 1, P = 1, D = 1, Q = 1, S = 12,plot.all = T)
checkresiduals(fit$fit) #good fit 

#plot
plot <- rbind(cbind(fortify(ice.series), sd = 0), cbind(fortify(forecast$pred), sd = as.numeric(forecast$se)))
plot$upper <- plot$y + plot$sd * 1.96
plot$lower <- plot$y - plot$sd * 1.96
plot$upper2<- plot$y + plot$sd * 1.282
plot$lower2<- plot$y - plot$sd * 1.282
ggplot(plot, aes(x = x ,y = y)) + 
  geom_line()+ geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,fill="deepskyblue3") + geom_ribbon(aes(ymin = lower2, ymax = upper2), alpha = 0.2,fill="deepskyblue3") +
  ylab("Ice Extent") + xlab("Year")+theme(panel.background = element_rect(fill = 'aliceblue'))+
  ggtitle("Forecast from ARIMA(2,1,1)(1,1,1)[12]")+theme(plot.title = element_text(hjust = 0.5))

#ARIMAX Model - exogeneous variable 
temp=data$temp
temp= as.vector(temp)

temperature=ts(temp,start=c(2006,1),frequency = 12)
plot(temperature)

t.training=head(temp,length(training))
t.test=tail(temp,length(test))

sarimax=Arima(training,order=c(2L,1L,1L),seasonal = list(order=c(1L,1L,1L),frequency=12),xreg = t.training)
sarimax

forecast7=forecast::forecast(sarimax,h=length(t.test),xreg=t.test)
forecast7
autoplot(forecast7) 
autoplot(forecast7)+autolayer(test,series = "Test Data")

accuracy(forecast7,ice.series)#best MAPE 

#Forecasting the SARIMAX Model
sarimax=Arima(ice.series,order=c(2L,1L,1L),seasonal = list(order=c(1L,1L,1L),freq=12),xreg= temperature)

#forecasting temp first 
#checking if additive/multiplicative 
ets=ets(temperature)
ets 
#(error,trend,seasonal)
#A- addtive, N- Null, M- Multiplicative
#ANA reflects addtive model
BoxCox.lambda(temperature)#lamda= 1 (no transformation)

#components of ts 
decompose=decompose(temperature)
plot(decompose)
seasonal=decompose$seasonal
plot(seasonal)
trend=decompose$trend
plot(trend) 

#stationary test 
adf.test(temperature)
ndiffs(temperature)
nsdiffs(temperature)
kpss.test(ice.series,null="Level") #not mean stationary 
#kpss.test(ice.series,null="Trend") #deterministic trend

#checking for 1st difference 
t.df=diff(temperature)
adf.test(t.df)
var(temperature)>var(t.df)

#test and training data 
t.training=window(temperature,end=c(2017,12))
t.test=window(temperature,start=c(2018,1),end=c(2020,8))

#making it staionary 
adf.test(t.training)
ndiffs(t.training)
nsdiffs(t.training)
ets(t.training)
td.training=diff(t.training)
td.test=diff(t.test)
adf.test(td.training)

#Holtwinters - not happening 
holtwinter=HoltWinters(t.training,gamma=F)
holtwinter
forecast1t=forecast:::forecast.HoltWinters(holtwinter,h=32)
plot(forecast1t)

#check
autoplot(forecast1t)+autolayer(t.test,series = "Test Data")

#Box Jenkins - ARIMA Model
acf=acf(t.training,lag.max = 36,plot=F)
acf$lag <- acf$lag * 12# transform lag from years to months
plot(acf)
pacf=pacf(t.training,lag.max = 36,plot=F)
pacf$lag=pacf$lag*12 
plot(pacf)
arima=auto.arima(t.training)
arima
forecast2t=forecast(arima,h=32)
plot(forecast2t)
checkresiduals(arima)

#check
autoplot(forecast2t)+autolayer(t.test,series = "Test Data")

#ets 
ets=ets(t.training)
forecast3t=forecast.ets(ets,h=32)
plot(forecast3t)

#check
autoplot(forecast3t)+autolayer(t.test,series = "Test Data")

#checking accuracy- use MAPE to check test value
accuracy(forecast1t,temperature) #holtwinters
accuracy(forecast2t,temperature) #auto.arima
accuracy(forecast3t,temperature) #ets - best


#choosing arima because of xreg argument
t.arima=Arima(temperature,order=c(4L,1L,1L),seasonal=c(2L,0L,0L))
t.forecast=forecast(t.arima,h=12)
plot(t.forecast)
t.temp=(t.forecast$mean)

#forecasting the final model 
forecast=forecast(sarimax,h=length(t.temp),xreg=t.temp)
autoplot(forecast)
checkresiduals(forecast)
#find MSE for both find

#VAR Model 
ice.series
temperature
b.data=ts.union(ice.series,temperature)
theme_set(theme_bw())
autoplot(b.data) +
  ggtitle("Time Series Plot of the `Ice Extent and Temperature' Time-Series") +
  theme(plot.title = element_text(hjust = 0.5)) #for centering the text - not applicable as different scale
apply(b.data, 2, adf.test) #checking stationarity 
stnry=diffM(b.data) #making the mts stationary
apply(stnry, 2, adf.test)#checking again
apply(stnry, 2, adf.test)
var=vars::VAR(stnry) #fit
summary(var)
serial.test(var) #check residuals
causality(var,
          cause = c("temperature")) #cause variable. If not specified then first column of x is used. Multiple variables can be used. 
fcast = predict(var.a, n.ahead = 25) # we forecast over a short horizon because beyond short horizon prediction becomes unreliable or uniform

theme_set(theme_economist())
checkresiduals(holtwinters) #use my code 
checkresiduals(ets) 
checkresiduals(sarimax) #final model NOT on trsining


bats=bats(training,use.box.cox = NULL,use.trend = NULL,use.damped.trend = NULL,seasonal.periods = 12,use.arma.errors = TRUE)
forecast8=forecast(bats,h=32)

autoplot(forecast8,lwd=0.73,fcol="dodgerblue4",flwd=0.73,showgap=F)+autolayer(test,series = "Test Data",color="indianred3",lwd=0.72)+theme(panel.background = element_rect(fill = 'aliceblue')) +xlab("Year")+ylab("Training series (in sq km)")
1e-04

#plots

#main ts 
autoplot(ts(ice.extent,start=c(2006,1),frequency = 12)) + geom_line(color="blue4", size=0.7)+xlab("Year")+ylab("Ice Cover (sq km)")+theme_economist(base_size = 14) + scale_color_economist()+ggtitle("Time Series Plot")+theme(plot.title = element_text(hjust = 0.5))

#holtwinters
autoplot(forecast1,lwd=0.73,fcol="dodgerblue4",flwd=0.73,showgap=F)+autolayer(test,series = "Test Data",color="indianred3",lwd=0.72)+theme(panel.background = element_rect(fill = 'aliceblue')) +xlab("Year")+ylab("Training series (in sq km)")+theme(plot.title = element_text(hjust = 0.5))

#auto.sarima
autoplot(forecast2,lwd=0.73,fcol="dodgerblue4",flwd=0.73,showgap=F)+autolayer(test,series = "Test Data",color="indianred3",lwd=0.72)+theme(panel.background = element_rect(fill = 'aliceblue')) +xlab("Year")+ylab("Training series (in sq km)")+theme(plot.title = element_text(hjust = 0.5))

#sarima 1 
autoplotplot <- rbind(cbind(fortify(training), sd = 0), cbind(fortify(forecast5$pred), sd = as.numeric(forecast5$se)))
plot$upper <- plot$y + plot$sd * 1.96
plot$lower <- plot$y - plot$sd * 1.96
plot$upper2<- plot$y + plot$sd * 1.282
plot$lower2<- plot$y - plot$sd * 1.282
ggplot(plot, aes(x = x ,y = y)) + 
  geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,fill="deepskyblue3") + geom_ribbon(aes(ymin = lower2, ymax = upper2), alpha = 0.2,fill="deepskyblue3") +
  ylab("Training series (in sq km)") + xlab("Year")+theme(panel.background = element_rect(fill = 'aliceblue'))+
  ggtitle("Forecast from ARIMA(1,1,2)(0,1,1)[12]")+theme(plot.title = element_text(hjust = 0.5)) +
  autolayer(test,series = "Test Data",color="indianred3",lwd=0.73)


#sarima 2 
plot <- rbind(cbind(fortify(training), sd = 0), cbind(fortify(forecast6$pred), sd = as.numeric(forecast6$se)))
plot$upper <- plot$y + plot$sd * 1.96
plot$lower <- plot$y - plot$sd * 1.96
plot$upper2<- plot$y + plot$sd * 1.282
plot$lower2<- plot$y - plot$sd * 1.282
ggplot(plot, aes(x = x ,y = y)) + 
  geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,fill="deepskyblue3") + geom_ribbon(aes(ymin = lower2, ymax = upper2), alpha = 0.2,fill="deepskyblue3") +
  ylab("Training series (in sq km)") + xlab("Year")+theme(panel.background = element_rect(fill = 'aliceblue'))+
  ggtitle("Forecast from ARIMA(2,1,1)(1,1,1)[12]")+theme(plot.title = element_text(hjust = 0.5)) +
  autolayer(test,series = "Test Data",color="indianred3",lwd=0.73)


#ets - NA 
autoplot(forecast3,lwd=0.73,fcol="dodgerblue4",flwd=0.73,showgap=F)+autolayer(test,series = "Test Data",color="indianred3",lwd=0.72)+theme(panel.background = element_rect(fill = 'aliceblue')) +xlab("Year")+ylab("Training series (in sq km)")

#arimax 
autoplot(forecast7,lwd=0.73,fcol="dodgerblue4",flwd=0.73,showgap=F)+autolayer(test,series = "Test Data",color="indianred3",lwd=0.72)+theme(panel.background = element_rect(fill = 'aliceblue')) +xlab("Year")+ylab("Training series (in sq km)")

#final forecast 
autoplot(forecast7,lwd=0.73,fcol="dodgerblue4",flwd=0.73,showgap=F)+theme(panel.background = element_rect(fill = 'aliceblue')) +xlab("Year")+ylab("Training series (in sq km)")


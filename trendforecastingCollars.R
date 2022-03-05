install.packages("rio")
install.packages('reshape2')
install.packages("gridExtra")
library('ggplot2')
library('forecast')
library('tseries')
library('tidyverse')
library('rio')
library(reshape2)
library(readxl)
library(gridExtra)
data1 <- import("F:/NIFT/Graduation Project/multiTimeline (2).csv")
view(data1)

summary(data1)

#data1$Week <- as.Date(data1$date)

head(data1)

(p1<- ggplot(data1, aes(Week, crew)) + 
  geom_line() + scale_x_date("Time in Year")+ 
  ylab("Crew Neck") + xlab(""))

(p2<- ggplot(data1, aes(Week, polo)) + 
  geom_line() + scale_x_date("Time in Year")+ 
  ylab("Polo Collar") + xlab(""))

(p3<- ggplot(data1, aes(Week, mandarin)) + 
  geom_line() + scale_x_date("Time in Year")+ 
  ylab("Mandarin Collar") + xlab(""))


plots <- list(p1,p2,p3)

layout <- rbind(c(1,1),c(2,2),c(3,3))

grid.arrange(grobs = plots, layout_matrix = layout, top="Past 5 years data of Collars/trends")
#?grid.arrange

crew_ts<- ts(data1[,c("crew")])
polo_ts<- ts(data1[,c("polo")])
mandarin_ts<- ts(data1[,c("mandarin")])


data1$clean_crew <- tsclean(crew_ts)
summary(data1$clean_crew)
summary(data1$crew)

data1$clean_polo <- tsclean(polo_ts)
summary(data1$clean_polo)
summary(data1$polo)

data1$clean_mandarin <- tsclean(mandarin_ts)
summary(data1$clean_mandarin)
summary(data1$mandarin)


(p1<- ggplot(data1, aes(Week, clean_crew)) + 
  geom_line() + scale_x_date("Time in Year")+ 
  ylab("Crew Neck") + xlab(""))

(p2<- ggplot(data1, aes(Week, clean_polo)) + 
  geom_line() + scale_x_date("Time in Year")+ 
  ylab("Polo Collar") + xlab(""))

(p3<- ggplot(data1, aes(Week, clean_mandarin)) + 
  geom_line() + scale_x_date("Time in Year")+ 
  ylab("Mandarin Collar") + xlab(""))


plots <- list(p1,p2,p3)

layout <- rbind(c(1,1),c(2,2),c(3,3))

grid.arrange(grobs = plots, layout_matrix = layout, top="Data without outliers")

#ggplot(data1, aes(Week, clean_vneck)) + 
 # geom_line() + scale_x_date("Time in Year")+ 
  #ylab("V Neck") + xlab("")

data1$crew_ma = ma(data1$clean_crew, order = 7)
data1$crew_ma30 <- ma(data1$clean_crew, order = 30)

data1$polo_ma = ma(data1$clean_polo, order = 7)
data1$polo_ma30 <- ma(data1$clean_polo, order = 30)

data1$mandarin_ma = ma(data1$clean_mandarin, order = 7)
data1$mandarin_ma30 <- ma(data1$clean_mandarin, order = 30)


(p1<- ggplot()+ 
  geom_line(data=data1, aes(x=Week, y=clean_crew, colour="Counts")) +
  geom_line(data=data1, aes(x=Week, y=crew_ma, colour="Moving Average(w)")) +
  geom_line(data=data1, aes(x=Week, y=crew_ma30, colour="Moving Average(m)")) +
  ylab("Crew Neck"))

(p2<- ggplot()+ 
  geom_line(data=data1, aes(x=Week, y=clean_polo, colour="Counts")) +
  geom_line(data=data1, aes(x=Week, y=polo_ma, colour="Moving Average(w)")) +
  geom_line(data=data1, aes(x=Week, y=polo_ma30, colour="Moving Average(m)")) +
  ylab("Polo Collar"))

(p3<- ggplot()+ 
  geom_line(data=data1, aes(x=Week, y=clean_mandarin, colour="Counts")) +
  geom_line(data=data1, aes(x=Week, y=mandarin_ma, colour="Moving Average(w)")) +
  geom_line(data=data1, aes(x=Week, y=mandarin_ma30, colour="Moving Average(m)")) +
  ylab("Mandarin Collar"))



plots <- list(p1,p2,p3)

layout <- rbind(c(1,1),c(2,2),c(3,3))

grid.arrange(grobs = plots, layout_matrix = layout, top="Monthly and Weekly average")

crew_ma = ts(na.omit(data1$crew_ma), frequency = 30)
decomp = stl(crew_ma, s.window = "periodic")
deseasosanl_crew <- seasadj(decomp)
p1<- plot(decomp)

polo_ma = ts(na.omit(data1$polo_ma), frequency = 30)
decomp = stl(polo_ma, s.window = "periodic")
deseasosanl_polo <- seasadj(decomp)
p2<- plot(decomp)

mandarin_ma = ts(na.omit(data1$mandarin_ma), frequency = 30)
decomp = stl(mandarin_ma, s.window = "periodic")
deseasosanl_mandarin <- seasadj(decomp)
p3<- plot(decomp)



adf.test(crew_ma,alternative = "stationary")
adf.test(polo_ma,alternative = "stationary")
adf.test(mandarin_ma,alternative = "stationary")
adf.test(henley_ma,alternative = "stationary")
#adf.test(vneck_ma,alternative = "stationary")

#if p value is higher than 0.05, we reject null hypothesis
#to fix this we calculate the simple difference/ log/ log difference
#the number of I in ARIMA
#Autocorrelation and choosing order
# ACF AND PACF PLot Helps

#Acf(crew_ma, main ="")
#pacf(crew_ma, main="")

#crew_d1 = diff(deseasosanl_crew, diff=1)
#plot(crew_d1)
#adf.test(crew_d1,alternative = "stationary")
#Acf(crew_d1, main="ACF for differeced series")
#pacf(close_d1, main="Pacf for differenced series")

auto.arima(deseasosanl_crew, seasonal = FALSE)
auto.arima(deseasosanl_polo, seasonal = FALSE)
auto.arima(deseasosanl_mandarin, seasonal = FALSE)

#write the equation it gives for the data
#Yt = ar1*yt-1 + ar2*yt-2 + ma1*e1 + ma2*e2 + ma3*e3 + E
#(p= lag, d=difference, q=ma)
#target is too lower the AIC value

#crew

fit<- auto.arima(deseasosanl_crew, seasonal = FALSE)
#?tsdisplay
tsdisplay(residuals(fit), lag.max=45, main= '(2,1,4) Model Residulas')
#?arima
fit2<-arima(deseasosanl_crew, order = c(5,1,4))
fit2
tsdisplay(residuals(fit), lag.max=45, main= '(5,1,4) Model Residulas')

fcast<- forecast(fit2, h=30)
plot(fcast)

#polo
fit<- auto.arima(deseasosanl_polo, seasonal = FALSE)
#?tsdisplay
tsdisplay(residuals(fit), lag.max=45, main= '(2,1,4) Model Residulas')
#?arima
fit2<-arima(deseasosanl_polo, order = c(5,1,4))
fit2
tsdisplay(residuals(fit), lag.max=45, main= '(5,1,4) Model Residulas')

fcast<- forecast(fit2, h=30)
plot(fcast)

#mandarin
fit<- auto.arima(deseasosanl_mandarin, seasonal = FALSE)
#?tsdisplay
tsdisplay(residuals(fit), lag.max=45, main= '(2,1,4) Model Residulas')
#?arima
fit2<-arima(deseasosanl_mandarin, order = c(5,1,4))
fit2
tsdisplay(residuals(fit), lag.max=45, main= '(5,1,4) Model Residulas')

fcast<- forecast(fit2, h=30)
plot(fcast)


#crew
hold<- window(ts(deseasosanl_crew), start = 245)
fit_no_holdout = arima(ts(deseasosanl_crew[-c(245:261)]), order = c(5,1,4))
fcast_no_holdout <- forecast(fit_no_holdout, h=30)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasosanl_crew))

fit_w_seasonal <- auto.arima(deseasosanl_crew, seasonal = TRUE)
fit_w_seasonal

sea_forecast <- forecast(fit_w_seasonal, h=30)
p1<- plot(sea_forecast)

#polo
hold<- window(ts(deseasosanl_polo), start = 245)
fit_no_holdout = arima(ts(deseasosanl_polo[-c(245:261)]), order = c(5,1,4))
fcast_no_holdout <- forecast(fit_no_holdout, h=30)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasosanl_polo))

fit_w_seasonal <- auto.arima(deseasosanl_polo, seasonal = TRUE)
fit_w_seasonal

sea_forecast <- forecast(fit_w_seasonal, h=30)
p2<- plot(sea_forecast)

#mandarin
hold<- window(ts(deseasosanl_mandarin), start = 245)
fit_no_holdout = arima(ts(deseasosanl_mandarin[-c(245:261)]), order = c(5,1,4))
fcast_no_holdout <- forecast(fit_no_holdout, h=30)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasosanl_mandarin))

fit_w_seasonal <- auto.arima(deseasosanl_mandarin, seasonal = TRUE)
fit_w_seasonal

sea_forecast <- forecast(fit_w_seasonal, h=30)
p3<- plot(sea_forecast)




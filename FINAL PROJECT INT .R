install.packages("tidyverse")
install.packages("ggcorrplot")
install.packages("wesanderson")
install.packages("corrplot")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("date")
install.packages("lubridate")
install.packages("pacman")
install.packages("forecast")
install.packages("plotly")
install.packages("reshape2")
install.packages("TTR")
install.packages("scales")
install.packages("zoo")
library(zoo)
library(scales)
library(tseries)
install.packages("tseries")
library(lubridate)
library(TTR)
library(reshape2)
library(grid)
library(gridExtra)
library(date)
library(pacman)
library(forecast)
library(plotly)
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyverse)
library(ggcorrplot)
library(wesanderson)

#READ AND LOAD THE DATASET
read.csv(file.choose())
read.csv(file.choose())
Washington_Bike_Share <- read_csv(file.choose())

#ATTACHING THE DATASET
attach(Washington_Bike_Share)

# VIEW THE DATASET
View(Washington_Bike_Share)

#GETTING THE SUMMARY FOR DATASET
summary(Washington_Bike_Share)
str(Washington_Bike_Share)
head(Washington_Bike_Share)
tail(Washington_Bike_Share)
print(head(Washington_Bike_Share))

#scatterplot to show relationship between count(number of total rentals) and temperature
ggplot(data = Washington_Bike_Share, aes(temp,cnt)) + geom_point(alpha=0.3, aes(color= temp)) + theme_light()

#scatterplot for relationship between count and date 
Washington_Bike_Share$dteday <- as.POSIXct(Washington_Bike_Share$dteday)
pl <- ggplot(Washington_Bike_Share,aes(dteday,cnt)) + geom_point(aes(color=temp),alpha=0.5)
pl + scale_color_continuous(low = '#55D8CE',high = '#FF6E2E') + theme_classic()

#Cor relation between temperature and count
cor(Washington_Bike_Share[,c('temp','cnt')])
#Boxplot
ggplot(Washington_Bike_Share,aes(factor(season),cnt)) + geom_boxplot(aes(color = factor(season))) + theme_dark()

#Relationship between hour of the working day and count of rented bikes
pl1 <- ggplot(filter(Washington_Bike_Share,workingday == 1), aes(hr,cnt))
pl1 <- pl1+ geom_point()
print(pl1)

#temperature and bike rental count
pl1 <- pl1 + geom_point(position=position_jitter(w=1,h=0),aes(color = temp),alpah=0.5)
pl1 <- pl1 + scale_color_gradientn(colours = c('blue','pink','light blue','light green','yellow','orange','red'))
print(pl1 + theme_bw())

#Relationship Between hour of non working day and count of rented bikes
pl2 <- ggplot(filter(Washington_Bike_Share,workingday == 0), aes(hr,cnt))
pl2 <- pl2+ geom_point()
pl2 <- pl2 + geom_point(position=position_jitter(w=1,h=0),aes(color = temp),alpah=0.5)
pl2 <- pl2 + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
print(pl2 + theme_bw())
names(Washington_Bike_Share)

knitr::opts_chunk$set(echo = TRUE)

# Show the bike rental trend annually
ggplot(data=Washington_Bike_Share, aes(x=mnth, y=cnt, fill= yr)) + scale_x_continuous(breaks = seq(1,12,by=1)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs( x= "mnth", y="Rental Count", fill= "yr")

#bike rental count with respect to weather conditions
print("With Humidity:")
cor(Washington_Bike_Share$hum,Washington_Bike_Share$cnt)
print("With Temperature:")
cor(Washington_Bike_Share$cnt,Washington_Bike_Share$temp)
print("With Real Feel Temperature:")
cor(Washington_Bike_Share$cnt,Washington_Bike_Share$atemp)
print("With Windspeed:")
cor(Washington_Bike_Share$cnt,Washington_Bike_Share$windspeed)

#Plots for the weather conditions
f1 <- ggplot(data=Washington_Bike_Share, aes(x=hum, y=cnt)) +geom_point(size=0.00001)+ labs(x= "Humidity", y="Rental Count")  +scale_x_continuous(breaks=c(0,0.5,1))
f2 <- ggplot(data=Washington_Bike_Share, aes(x=temp, y=cnt)) +geom_point(size=0.00001)+ labs(x= "Temperature", y="Rental Count")  +scale_x_continuous(breaks=c(0,0.5,1)) + geom_smooth(method='lm')
f3 <- ggplot(data=Washington_Bike_Share, aes(x=atemp, y=cnt)) +geom_point(size=0.00001)+ labs(x= "Real feel Temperature", y="Rental Count")  +scale_x_continuous(breaks=c(0,0.5,1)) + geom_smooth(method='lm')
f4 <- ggplot(data=Washington_Bike_Share, aes(x=windspeed, y=cnt)) +geom_point(size=0.00001)+ labs(x= "Windspeed", y="Rental Count")  +scale_x_continuous(breaks=c(0,0.5,1)) + geom_smooth(method='lm')
grid.arrange(f1, f2, f3, f4, ncol=2, nrow=2)

# Plot for weather conditions affecting the bike rentals 
stacked_plot_data = Washington_Bike_Share %>% select(holiday,weekday,cnt,casual,registered,weathersit) %>%  
  mutate(public_holiday = ifelse(holiday == 0 ,"Non-Working Day", "Working Day"))
stacked_plot_data = stacked_plot_data %>% select(holiday,weekday,cnt,casual,registered,weathersit, public_holiday) %>% mutate(weather = case_when(weathersit==1 ~ "Clear" , weathersit==2 ~ "Mild" ,  weathersit==3 ~ "Snow/Rain"))

#plot for effect of weather on sales  
ggplot(stacked_plot_data, aes(x = weathersit, y = cnt)) +
  geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma) + labs(x= "Weather Condition", y="Rental Count")

ggplot() + geom_bar(aes(y = cnt, x = as.factor(weekday), fill = as.factor(public_holiday)), data = stacked_plot_data,stat="identity",position=position_dodge()) + labs( x= "Day", y="Rental Count" , fill="Holiday") + scale_y_continuous(labels = scales::comma)+ scale_x_discrete(labels= c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))

#Plot wrt casual and registered users 
f1 <- ggplot(stacked_plot_data, aes(x = as.factor(weekday), y = casual, group=1))+ scale_x_discrete(labels= c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))+ 
  geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma)+ labs(x= "Day", y="Casual Users Count")

f2 <- ggplot(stacked_plot_data, aes(x = as.factor(weekday), y = registered, group=1)) + scale_x_discrete(labels= c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))+
  geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma) + labs(x= "Day", y="Registered Users Count")
grid.arrange(f1, f2, ncol=2)


# CO relation Plot
Washington_Bike_Share_cor<- Washington_Bike_Share %>% select (cnt,atemp,temp,hum,windspeed)
Washington_Bike_Share_cor<- data.frame(Washington_Bike_Share_cor)

colnames(Washington_Bike_Share_cor)[1] <- "Total Number of Bike Rentals"
colnames(Washington_Bike_Share_cor)[2] <- "Temperature"
colnames(Washington_Bike_Share_cor)[3] <- "Feel Temperature"
colnames(Washington_Bike_Share_cor)[4] <- "Humidity"
colnames(Washington_Bike_Share_cor)[5] <- "Windspeed"

A<-cor(Washington_Bike_Share_cor)
corrplot(A, method="number")

#heatmap
correlation <- mutate_all(day, function(x) as.numeric(as.character(x)))
df<- cor(day[,3:16])
corrplot(cor(df), method = 'number')

#Performing Linear Modelling
model <-lm(cnt~atemp+hum+windspeed+season+weathersit,data= Washington_Bike_Share)
test_data = data.frame(atemp=0.165,hum=0.3,windspeed=0.4,season=1,weathersit=1)
summary(model)

#performing Time Series Analysis
str(Washington_Bike_Share)
as.Date(Washington_Bike_Share$dteday)

  

Y <- Washington_Bike_Share$dteday


Washington_Bike_Share$ReportDate = as.Date(Washington_Bike_Share$dteday, format = "%m/%d/%Y %I:%M:%S %p") 
Washington_Bike_Share$ReportDate = as.Date(format(Washington_Bike_Share$dteday, "%Y-%m-%d"))
Washington_Bike_Share$ReportDate

  
Washington_Bike_Share$year <- as.numeric(format(Washington_Bike_Share$ReportDate,'%Y'))

Washington_Bike_Share$day <- as.numeric(format(Washington_Bike_Share$ReportDate,'%d'))
Washington_Bike_Share$month <- as.numeric(format(Washington_Bike_Share$ReportDate,'%m'))

# Time Series on basis of Lineaer Modelling
timeseries_model <- lm(cnt ~ lag(cnt) + 
                         lag(cnt,2) +
                         lag(cnt,3) +
                         lag(cnt,4) +
                         lag(cnt,5), data = Washington_Bike_Share)
timeseries_model
Washington_Bike_Share%>% transmute(dteday,cnt,prediction = c(0,0,0,0,0, predict(timeseries_model))) %>%
  gather("Type","Value",-dteday) %>%
  ggplot(aes(x = dteday, y = Value, color = Type)) + 
  labs(x='Date')+
  geom_point()

count <- Washington_Bike_Share$cnt %>%
  ts(frequency = 365) %>%
  stl("periodic")

plot(forecast(count)) 
head(Washington_Bike_Share)

## Time series analysis
read.csv(file.choose())
day<-read.csv(file.choose())

#SPLITTING DATA INTO TEST AND TRAIN KEEPING 80 AND 20%.
data_ts <-msts(day[,'cnt'], seasonal.periods=c(7))
train_ts <- head(data_ts, round(length(data_ts) * 0.8))
test_ts <- tail(data_ts, round(length(data_ts) * 0.2))
plot(train_ts, xlab="Weeks", ylab="Bike riders")

#DECOMPOSING THE TIME SERIES FOR THE DATASET
plot(decompose(data_ts, type='add'), xlab="Weeks")

#TEST FOR STATIONARITY 
adf_test <- adf.test(data_ts, alternative='stationary')
print(adf_test)

#ACF AND PACF FOR SEASONAL TIME SERIES 
acf_ts <- acf(data_ts[1:length(train_ts)], plot = FALSE)
plot(acf_ts,  main = "Autocorrelation function")

pacf_ts <- pacf(train_ts[1:length(train_ts)], plot = FALSE)
plot(pacf_ts,  main = " Partial autocorrelation function")

## Non-Stationary to Stationary by Seasonal Difference
stat_diff <- diff(train_ts, differences = 1)
plot(stat_diff, main = " Seasonal Difference", xlab= "Weeks")
adf_test <- adf.test(stat_diff, alternative = 'stationary')
adf_test

## ACF and PACF for non seasonal series 
acf_ts <- acf(stat_diff[1:length(stat_diff)], plot = FALSE)
plot(acf_ts,  main = "Autocorrelation function")

pacf_ts <- pacf(stat_diff[1:length(stat_diff)], plot = FALSE)
plot(pacf_ts,  main = " Partial autocorrelation function")

## ARIMA Forecasting 
fit1 <- Arima(train_ts, order=c(7,1,0),seasonal=c(6,1,0), method = "CSS", optim.method = "BFGS")
forecast_ts <- forecast(fit1,h= length(test_ts))
autoplot(forecast_ts, xlab="Weeks", ylab= "Bike Riders") + autolayer(test_ts)
accuracy(forecast_ts, test_ts)


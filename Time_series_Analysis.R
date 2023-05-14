#Creating a DataFrame from our dataset
mydata <- read.csv("C:\\Users\\Prakruthi\\Documents\\R files\\opsd_germany_daily.txt", header=TRUE, row.names="Date")
mydata

#Printing part of DataFrame using head() and tail()
head(mydata)
tail(mydata)

#view data in tabular format
View(mydata)

#Retrieve the dimensions of object
dim(mydata)

#summary of data
summary(mydata)

#Datatype of each column in dataframe
str(mydata)

#Looking for date column (will not show data as it is created index)
head(mydata$Date)

#Print row names
row.names(mydata)

#accessing a specific row
mydata["2006-08-12",]

mydata["2015-10-21",]

#accessing multiple rows
mydata[c("2006-08-12","2015-10-21"),]

summary(mydata)

#Without parsing date column
mydata2 <- read.csv("C:\\Users\\Prakruthi\\Documents\\R files\\opsd_germany_daily.txt", header=TRUE)
mydata2

#Date column
str(mydata2$Date)

#Convert into date format
x <- as.Date(mydata2$Date)
head(x)
class(x)
str(x)

#Create year, month, day, weekdays columns
year <- as.numeric(format(x,'%Y'))
head(year)

month <- as.numeric(format(x,'%m'))
head(month)

day <- as.numeric(format(x,'%d'))
head(day)

weekday <- weekdays(as.Date(x,'%w'))

head(mydata2)

#Add columns to the existing dataframe
mydata2 <- cbind(mydata2, year, month, day, weekday)
head(mydata2)

mydata2[1:3,]

set.seed(0)
sampled_data <- mydata2[sample(nrow(mydata2), 20), ]
sampled_data

#visualization of data using plot()

#option 1:
plot(mydata2$year, mydata2$Consumption, type='l', xlab='year', ylab='Consumption')

#option 2:
plot(mydata2$year, mydata2$Consumption, type='l', xlab='year', ylab='Consumption'
     ,lty=1,ylim=c(800,1700), xlim=c(2006,2018))

#option 3:
par(mfrow=c(1,1))
plot(mydata2[,2])

#option 4:
plot(mydata2[,2], xlab='year',ylab='Consumption')
plot(mydata2[,2], xlab='year',ylab='Consumption', type='l',lwd=2, col='blue')
plot(mydata2[,2], xlab='year',ylab='Consumption', type='l',lwd=2, xlim=c(0,2018))
plot(mydata2[,2], xlab='year',ylab='Consumption', type='l',lwd=2, xlim=c(2006,2018),
     ylim = c(900, 2000), main = "Consumption Graph")

#Taking log values of consumption and take differences of logs
plot(10*diff(log(mydata2[,2])),xlab="year",ylab="Consumption",type = 'l',lwd=2,
     ylim = c(-5,5),main = "Consumption Graph",col="orange")

#Plot using ggplot()
#install.packages("ggplot2")
library(ggplot2)

ggplot(data=mydata2, aes(x=year, y=Consumption, col='red')) + geom_point()

#Plot the data considering the solar and wind time series

#Wind column
min(mydata2[,3],na.rm = T)
max(mydata2[,3],na.rm = T)

#consumption column
min(mydata2[,2], na.rm=T)
max(mydata2[,2], na.rm=T)

#solar column
min(mydata2[,4], na.rm=T)
max(mydata2[,4], na.rm=T)

#wind + solar
min(mydata2[,5], na.rm=T)
max(mydata2[,5], na.rm=T)

mydata2[,1] <- as.Date(mydata2[,1])
View(mydata2)

#for multiple plots
par(mfrow=c(3,1))

#Consumption

plot1 <- plot(mydata2[,1],mydata2[,2],xlab="year",ylab = "Daily Total (Gwh)", type="l",
              lwd=0.5, main="Consumption",col="orange", ylim=c(840,1750))

#Solar

plot2 <- plot(mydata2[,1],mydata2[,4],xlab="year",ylab = "Daily Total (Gwh)", type="l",lwd=0.5,
              main="Solar",col="blue", ylim=c(0,500))

#Wind

plot3 <- plot(mydata2[,1],mydata2[,3],xlab="year",ylab = "Daily Total (Gwh)", type="l",lwd=0.5,
              main="Wind",col="red", ylim=c(0,500))

par(mfrow=c(1,1))

#Extract data for particular year
mydata3 <- subset(mydata2, subset = mydata2$Date >= '2017-01-01' & mydata2 <= '2017-12-31')
head(mydata3)

plot4 <-  plot(mydata3[,1],mydata3[,2],xlab="year",ylab = "Daily Total (Gwh)", type="l",lwd=1,
               main="Wind",col="purple")

#Zooming in further
mydata3 <- subset(mydata2, subset=mydata2$Date >= '2017-01-01' & mydata2$Date <= '2017-02-18')
head(mydata3)

xmin <- min(mydata3[,1], na.rm = T)
xmax <- max(mydata3[,1], na.rm = T)
xmin
xmax

ymin <- min(mydata3[,2], na.rm = T)
ymax <- max(mydata3[,2],na.rm = T)
ymin
ymax

plot4 <- plot(mydata3[,1],mydata3[,2],xlab="year",ylab = "Daily Total (Gwh)", type="l",lwd=1,
              main="Wind",col="green", xlim = c(xmin, xmax), ylim = c(ymin, ymax))
grid()
#add solid horizontal lines at
abline(h=c(1300,1500,1600))
#add dashed blue vertical lines
abline(v=seq(xmin, xmax,7), lty=2, col="blue")

##################################

#Seasonality
#Boxplot
quantile(mydata2$Consumption, probs = c(0,0.25,0.5,0.75,1))
boxplot(mydata2$Consumption, main="Consumption", ylab="Consumption",
        ylim=c(600,1800), las=1)

#Yearly
boxplot(mydata2$Consumption ~ mydata2$year, main="Consumption",
        ylab = "Consumption", xlab="Years",
        ylim = c(600,1800))

#Monthly
boxplot(mydata2$Consumption ~ mydata2$month, main="Consumption",
        ylab = "Consumption", xlab="month",
        ylim = c(600,1800), las=1)

#Multiple plots
par(mfrow=c(3,1))

boxplot(mydata2$Consumption ~ mydata2$month, main="Consumption",
        ylab = "Consumption", xlab="month",
        ylim = c(600,1800), col=cm.colors(12), las=1)

boxplot(mydata2$Wind ~ mydata2$month, main="Wind",
        ylab = "Wind", xlab="month",
        ylim = c(0,900), col=heat.colors(12), las=1)

boxplot(mydata2$Solar ~ mydata2$month, main="Solar",
        ylab = "Solar", xlab="month",
        ylim = c(0,200), col=terrain.colors(12), las=1)

#Weekdays
par(mfrow=c(1,1))

boxplot(mydata2$Consumption ~ mydata2$weekday, main="Consumption",
        ylab = "Consumption", xlab="month",
        ylim = c(600,1800), col=rainbow(12), las=1)

#############################
library(dplyr)
summary(mydata2)
colSums(!is.na(mydata2))
colSums(is.na(mydata2))

#Frequencies
xmin <- min(mydata2[,1], na.rm = T)
xmin
freq1 <- seq(from=xmin, by="day", length.out=5)
freq1
freq2 <- seq(from=xmin, by="month", length.out=5)
freq2
freq3 <- seq(from=xmin, by="year", length.out=5)
freq3

#Let's select data which has NA values for wind
selWind1 <- mydata2[which(is.na(mydata2$Wind)),
                    names(mydata2) %in% c("Date","Consumption","Wind","Solar")]
selWind1[1:10,]

#Let's select data which does not have NA values for wind
selWind2 <- mydata2[which(!is.na(mydata2$Wind)),
                    names(mydata2) %in% c("Date","Consumption","Wind","Solar")]
selWind2[1:10,]
View(selWind2)

#Looking at Result of above two, we know that year 2011 has wind column with some missing values
selWind3 <- mydata2[which(mydata2$year == "2011"),
                    names(mydata2) %in% c("Date","Consumption","Wind","Solar")]
selWind3[1:10,]
class(selWind3)

#Number of rows in selWind3
nrow(selWind3)

#find number of NA values for a particular year
sum(is.na(mydata2$Wind[which(mydata2$year == "2011")]))

selWind3[which(is.na(selWind3$Wind)),
         names(selWind3) %in% c("Date","Consumption","Wind","Solar")]

#Let's select data which has na and non-na values
test1 <- selWind3[which(selWind3$Date > "2011-12-12" & selWind3$Date < "2011-12-16"),
         names(selWind3) %in% c("Date","Consumption","Wind","Solar")]
test1
class(test1)
str(test1)

library(tidyr)
test1 %>% fill(Wind)

#Thus we can fill missing values using direction

#######################################
#Trend analysis, looking at rolling mean
mydata2

threedayTest <- mydata2 %>%
  dplyr::arrange(desc(year)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(test_03da = zoo::rollmean(Consumption, k=3, fill=NA),
                ) %>%
  dplyr::ungroup()
View(threedayTest)

#Now let's calculate 7 day and 365 day rolling mean for consumption
mydataTest <- mydata2 %>%
  dplyr::arrange(desc(year)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(test_07da = zoo::rollmean(Consumption, k=7, fill=NA),
                test_365da = zoo::rollmean(Consumption, k=365, fill=NA)) %>%
  dplyr::ungroup()

#Check your result
mydataTest %>%
  dplyr::arrange(Date) %>%
  dplyr::filter(year == 2017) %>%
  dplyr::select(Consumption,
                Date,
                year,
                test_07da:test_365da) %>%
  utils::head(7)

#Visualize
par(mfrow=c(1,1))

plot(mydataTest$Consumption, xlab = "year", ylab="Consumption",type="l",
     lwd=1, col="skyblue3", main="Consumption Graph")

points(mydataTest$test_07da, type="l", lwd=2,
       xlim=c(2000,2018), ylim=c(900,2000), col="salmon3")

lines(mydataTest$test_365da, type="l", lwd=5,
      xlim=c(2000,2018),ylim=c(900,2000),col="black")

legend("topright", legend=c("mydataTest$Consumption", "mydataTest$test_07da", "mydataTest$test_365da"),
       lty=c(1,1,1), lwd=c(1,2,5), col=c("skyblue3", "salmon3", "black"))

#Similarly we can work on looking at trends for wind and solar data
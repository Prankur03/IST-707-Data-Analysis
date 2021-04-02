
setwd('C:/Users/prank/OneDrive/Desktop/Sem2/Data Analytics/Data Analytics/Project/Data')
Data2 <- read.csv("household_2010.csv")


summary(Data2)

# Dropping column 'x'
Data2$X <- NULL

# Replacing ? with NA
Data2[,3:9][Data2[,3:9] == "?"] <- NA


#summary(Data2)

# Finding NAs
sum(is.na(Data2$Date))
sum(is.na(Data2$Time))
sum(is.na(Data2$Sub_metering_1))
sum(is.na(Data2$Sub_metering_2))
sum(is.na(Data2$Sub_metering_3))
sum(is.na(Data2$Global_active_power))
sum(is.na(Data2$Global_reactive_power))
sum(is.na(Data2$Voltage))


# Removing NAs
Data2 <- na.omit(Data2)
#summary(Data2)

Data <- Data2


Data$Time <- as.character(Data$Time)
Data$Date <- as.character(Data$Date)
Data$Sub_metering_1 <- as.numeric(as.character(Data$Sub_metering_1))
Data$Sub_metering_2 <- as.numeric(as.character(Data$Sub_metering_2))
Data$Sub_metering_3 <- as.numeric(Data$Sub_metering_3)
Data$Global_active_power <- as.numeric(as.character(Data$Global_active_power))
Data$Global_reactive_power <- as.numeric(as.character(Data$Global_reactive_power))
Data$Voltage <- as.numeric(as.character(Data$Voltage))
Data$Global_intensity <- as.numeric(as.character(Data$Global_intensity))

str(Data)
###########################################################################################################
# Time series visualizations

library(ggplot2)
library(scales)


Data_timeS_viz <- Data


Data_timeS_viz$Date <- as.Date(Data_timeS_viz$Date, format="%Y-%m-%d")
Data_timeS_viz$Date_Time <- paste(Data_timeS_viz$Date, Data_timeS_viz$Time)
Data_timeS_viz$Date_Time <- strptime(Data_timeS_viz$Date_Time, "%Y-%m-%d %H:%M:%S")
Data_timeS_viz$Date_Time <- as.POSIXct(Data_timeS_viz$Date_Time, format="%Y-%m-%d T %H:%M:%S",tz="Europe/Paris")

str(Data_timeS_viz)

#Histogram of household global minute-averaged active power (in kilowatt)
with(Data_timeS_viz, hist(Global_active_power, main = "Global Active Power", 
                          xlab = "Global Active Power (kilowatts)", ylab = "Frequency", col = "red"))

# Interpretation
# We can see that we have a large number of observations for 0 to 500 watts, far fewer between 500 watts and 1 kilowatt and then a higher but steadily decreasing number of observations of over 1 kilowatt.
# One might speculate that this represents two different types of power consumption: the first when nobody is at home or occupants are asleep, and the second when household appliances are being used.


#Time series plot of global minute-averaged active power (in kilowatt) for 7 Days
Data_1week <- Data_timeS_viz[Data_timeS_viz$Date >= "2010-01-26" & Data_timeS_viz$Date <= "2010-01-31", ]

plot(x = Data_1week$Date_Time, y = Data_1week$Global_active_power, type = "l", 
     xlab = "Day/Time", ylab = "Global Active Power (kilowatts)")

# Interpretation
# We can see that power usage seems to roughly peak in the mornings and the evenings on weekdays, with higher usage on Friday evening, and all of Sunday.


# Time series plot of energy usage across various sections of the house for 1 week.
ggplot(data = Data_1week, aes(Data_1week$Date_Time))+
  geom_line(aes(y = Data_1week$Sub_metering_1, color="Sub metering 1")) + 
  geom_line(aes(y = Data_1week$Sub_metering_2, color="Sub metering 2")) + 
  geom_line(aes(y = Data_1week$Sub_metering_3, color="Sub metering 3")) + 
  xlab("Day/Time")+
  ylab("Global Active Power (kilowatts)")+
  ggtitle("Global Active Power by Time")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255)),guide='legend') 

# Interpretation
# Here, we can see that kitchen electricity usage is pretty low on Thursdays with higher usage on Friday evening, Saturday evening, and Sunday from the afternoon until the evening.
# By contrast, people seem to use their laundry rooms on Saturday morning, Saturday evening, and on Sunday around noon. Electric water heating and air-conditioning appear to have highest usage during Friday evening, Saturday daytime, and Sunday day time.

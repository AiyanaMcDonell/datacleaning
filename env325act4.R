#load packages
library("dplyr")
library("lubridate")
library("ggplot2")

#read in data
campusweather <- read.csv("/cloud/project/campus_weather.csv",
                          na.strings = "#N/A")
meterweather <- read.csv("/cloud/project/meter_weather_metadata.csv")
weather <- merge(campusweather, meterweather)

#before setting up a function, test out the operation to make sure it does what you want
interval <- campusweather$dateF[-length(campusweather$dateF)] %--% campusweather$dateF[-1]
interval

#set up a function to repeatedly check intervals in the data
#set up time intervals in a vector of dates
timeInterval <- function(x){
  x[-length(x)] %--% x[-1]
}

#parse date data, call date F for date formatted
campusweather$dateF <- mdy_hm(campusweather$Date)

#how to use a for loop (spaces important)
for(i in 1:6){
  print(paste("example", i))
}

seqEx <- c(1,4,6)
for(i in seqEx){
  print(paste("example", i))
}
#turn sequence into character string
chEx <- character()
for(i in 1:6){
  chEx[i] <- paste("example", i)
}

#remember to indicate what is going into each slot with [i], like in this numerical example
numEx <- numeric()
for(i in 1:6){
  numEx[i] <- 6*i
}




###activity 4 in class prompts
#1 - Calculate a rolling average of air temperatures over eight 15 min measurements (2 hours) for January of 2022 using a for loop.
#Make a plot of the 15 minute air temperature and the rolling average.

#create a month column
campusweather$month <- month(campusweather$dateF)
# create a year column
campusweather$year <- year(campusweather$dateF)

#filter to isolate january 2022
jan22 <- campusweather %>%
  filter(month == 1 & year == 2022)

#calc mean, then set up empty numeric vector
mean(jan22$AirTemp[1:8])

roll_ave_temp <- numeric()
for(i in 8:nrow(jan22)){
  roll_ave_temp[i] <- mean(jan22$AirTemp[(i-7):i])#() for order of operations
}

#create column in jan22
jan22$roll_ave_temp <- roll_ave_temp

###homework #4
#question 1: Indicate how many missing precipitation values are in your data. 
#note: Exclude precip with temp more than 2 and less than 0.

#create a flag for excluding precip with temp less than 0 and more than 2 degrees
campusweather$FreezeFlag <- ifelse(campusweather$AirTemp >= 0 & campusweather$AirTemp <= 2,
                                   NA,
                                   campusweather$AirTemp)

campusweather$FreezeFlag <- ifelse(campusweather$Precip <= 0 & campusweather$Precip >= 2,
                                   1,
                                   0)

#check flag
campusweather$FreezeFlag

#there are 39,850 missing/omitted entries


#question 2: Create a data flag that warns a user if the battery voltage falls below 8.5 Volts. 
#Explain how you set up the flag.
battery$FreezeFlag <- ifelse(battery$voltage <= 8.5, #check if voltage is at or below 8.5 Volts
                             1, #if true, set flag to 1
                             0 #if false set flag to 0)
                             
#i created the flag by using ifelse on the dataframe "battery" to check the voltage column.
#if a value is at or below 8.5 volts, the freezeflag column will assign either a 1 or 0
#if the value is true to the argument that i set.


#question 3: You should also create a function that checks for observations that are in unrealistic 
#data ranges in air temperature and solar radiation. 
#Explain how your function works.

#create a flag for unrealistic air temp ranges
campusweather$FreezeFlag <- ifelse(campusweather$AirTemp >= 0 & campusweather$AirTemp <= 20,
                                   NA,
                                   campusweather$AirTemp)


campusweather$FreezeFlag <- ifelse(campusweather$AirTemp <= 0 & campusweather$AirTemp >= 20,
                                   1,
                                   0)

#create a flag for unrealistic solar radiation ranges
campusweather$FreezeFlag <- ifelse(campusweather$SolRad >= 0 & campusweather$SolRad <= 12,
                                   NA,
                                   campusweather$SolRad)

campusweather$FreezeFlag <- ifelse(campusweather$SolRad >= 0 & campusweather$SolRad <= 12,
                                   1,
                                   0)

#just like the other flags, the argument is made that the data cannot be accepted if outside the range set,
#so 1 will be assigned in FreezeFlag column if data agrees/is true, and a 0 will be assigned if data
#is not true


#question 4: Make a plot of winter air temperatures in Jan - Mar of 2021. 
#Check for persistence issues that might indicate snow accumulation on the sensor. 
#Describe whether you think this might be an issue.

#make a df for air temps in Jan-Mar 2021
winter.air <- campusweather %>%
  filter(month %in% c(1,4) & year == 2021)

#plot winter.air
ggplot(data = winter.air, 
       aes(x = dateF, y = AirTemp))+
  geom_point()+
  labs(x = "Month", y = "Air Temperature", title = "Air Temperature at Hamilton College (Jan-Mar)")+
  theme_classic()

#There are no data values after February 1st which might represent snow accumulation on the sensor, which is an issue.

#question 5: You are asked for total daily precipitation in March and April of 2021. 
#Use a for loop to exclude (convert to NA) any days that include temperatures less than 35 degrees F on that day or the day prior to ensure that 
#measurements are not likely to be affected by snow accumulation on the sensor. 
#How many daily observations have precipitation observations (not a NA) in your final data table?

#first convert temperatures to F from C
campusweather$Fahrenheit <- campusweather$AirTemp * (9/5) + 32

#filter campus weather data for mar-apr 2021
march_april21 <- campusweather %>%
  filter(year == 2021 & month %in% c(3,4)) %>%
  group_by(doy) %>%
  summarize(daily_precip = sum(Precip, na.rm = TRUE), 
            min_temp = min(Fahrenheit, na.rm = TRUE))

#create function
for (i in 2:nrow(march_april21)) {
  current_temp <- march_april21$min_temp[i]
  previous_temp <- march_april21$min_temp[i-1]
  
  if(current_temp < 35 | previous_temp < 35) {
    march_april21$daily_precip[i] <- NA
  }
}

#sum of total daily precip 
total_precip <- sum(march_april21$daily_precip, na.rm = TRUE)

#number of rows without an observation (where value is NA)
sum(is.na(march_april21$daily_precip))

days_with_precip <- 61-42
#based on the function, there are 19 days with precipitation values (without NA values)


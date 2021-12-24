## install necessary packages
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("chron")
install.packages("scales")



## loading all packages

library(ggplot2)
library(tidyverse)
library(lubridate)
library(chron)   
library(scales)


## Reading the dataset
df_20_11 <-read.csv("R_Studio/datasets/trip_2020_11.csv")
df_20_12 <-read.csv("R_Studio/datasets/trip_2020_12.csv")
df_21_01 <-read.csv("R_Studio/datasets/trip_2021_01.csv")
df_21_02 <-read.csv("R_Studio/datasets/trip_2021_02.csv")
df_21_03 <-read.csv("R_Studio/datasets/trip_2021_03.csv")
df_21_04 <-read.csv("R_Studio/datasets/trip_2021_04.csv")
df_21_05 <-read.csv("R_Studio/datasets/trip_2021_05.csv")
df_21_06 <-read.csv("R_Studio/datasets/trip_2021_06.csv")
df_21_07 <-read.csv("R_Studio/datasets/trip_2021_07.csv")
df_21_08 <-read.csv("R_Studio/datasets/trip_2021_08.csv")
df_21_09 <-read.csv("R_Studio/datasets/trip_2021_09.csv")
df_21_10 <-read.csv("R_Studio/datasets/trip_2021_10.csv")


# convert start_station_id  & end_station_id from integer to character
df_20_11$start_station_id <- as.character(df_20_11$start_station_id)
df_20_11$end_station_id <- as.character(df_20_11$end_station_id)


# combine all files into 1 file
df <- bind_rows(df_20_11, df_20_12, df_21_01, df_21_02, df_21_03, df_21_04,
                df_21_05, df_21_06, df_21_07, df_21_08, df_21_09, df_21_10
                )


# double check the duplicate data and remove the duplicate 
df_distinct <- distinct(df)   # check any duplicate data



# check how many emtpy data for each column

df_na <- is.na(df)   # check any empty data
summary(df_na)        




# Create a column "day_of_week" and rideable_type,started_at, ended_at, 
# member_casual, ride_length, weekday, month_year, day_of_week from "df".
time_length <- df %>%
  mutate(day_of_week = as.character(wday(mdy_hms(started_at), week_start = 1))) %>%
  select(rideable_type,started_at, ended_at, member_casual, ride_length, weekday, month_year, day_of_week)


# A summary report for average ride length for both casual rider and member per month for last 12 months
month_member <- time_length %>%             # create the summary report
  group_by(month_year, member_casual) %>%
  summarise(avg = mean(times(ride_length)))

month_member_sorted <- arrange(month_member, my(month_year))     # sorted by month\year

mms_add <- month_member_sorted %>%                  # add an "Average time in minute" column to the report
  mutate(average_time = 60 * hours(times(avg)) + minutes(times(avg)) +  seconds(times(avg))/60)
  

# polt the data into graph
ggplot(data = mms_add) +
  geom_col(mapping = aes(x= my(month_year), y = average_time, fill = member_casual), position = "dodge") +
  #facet_grid(~member_casual) +
  labs(x = "Month/Year", y = "Minutes",  title = "Average Riding Time in Minute per Month", 
       subtitle ="These Average Riding Times are for last 12 months from 11/2020 to 10/2021") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y"))+
  scale_y_continuous(limits = c(0, 35),
                     breaks=seq(0, 35, 5),
                     labels = comma) +
  theme(axis.text.x = element_text(face="bold", color= "#0000FF", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#0000FF", 
                                   size=8, angle=0)) +
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="orange", size=14, face="bold"),
    axis.title.y = element_text(color="orange", size=14, face="bold")
  )


# ***********************************************************************************************************************

# A summary report for Average Riding Time for both Casual Rider and Member per day of week for last 12 months

weekday_member <- time_length %>%               # generate a summary report
  group_by(weekday, member_casual) %>%
  summarise(avg = mean(times(ride_length)))

# make an order from Monday to Sunday
weekday_member$weekday <- factor(weekday_member$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

weekday_member_sorted <- arrange(weekday_member, weekday)   # sort it by Monday through Sunday 

wms_add <- weekday_member_sorted %>%               # add a column of "Average time in minute" to the report
  mutate(average_time = 60 * hours(times(avg)) + minutes(times(avg)) +  seconds(times(avg))/60)


# plot the data into graph
ggplot(data = wms_add) +
  geom_col(mapping = aes(weekday, y = average_time, fill = member_casual), position = "dodge") +
  #facet_grid(~member_casual) +
  labs(x = "Day Of Week", y = "Minutes", 
       title = "Average Riding Time in Minutes from Monday to Sunday",
       subtitle = "These Average Riding Times are for last 12 months from 11/2020 to 10/2021") +
  scale_y_continuous(limits = c(0, 35),
                     breaks=seq(0, 35, 5),
                     labels = comma) +
  theme(axis.text.x = element_text(face="bold", color= "#0000FF", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#0000FF", 
                                   size=8, angle=0)) +
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="orange", size=14, face="bold"),
    axis.title.y = element_text(color="orange", size=14, face="bold")
  )


# *****************************************************************************************************************************

# A summary report for Total Rides for both Casual Rider and Member per day of week for last 12 months

total_rides_weekdays <- time_length %>%               # generate a summary report
  group_by(weekday, member_casual) %>%
  summarise(count = sum(!is.na(member_casual)))

# make an order from Monday to Sunday
total_rides_weekdays$weekday <- factor(total_rides_weekdays$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

total_rides_weekdays_sorted <- arrange(total_rides_weekdays, weekday)   # sort it by Monday through Sunday 

# plot the data into graph
ggplot(data = total_rides_weekdays_sorted) +
  geom_col(mapping = aes(weekday, y = count, fill = member_casual), position = "dodge") +
  #facet_grid(~member_casual) +
  labs(x = "Day Of Week", y = "Total Number of Rides", 
       title = "Total Number Of Rides: Casual Riders VS. Members",
       subtitle = "Total number of rides per day of week are for last 12 months from 11/2020 to 10/2021") +
  scale_y_continuous(limits = c(0, 600000),
                     breaks=seq(0, 600000, 100000),
                     labels = comma) +
  theme(axis.text.x = element_text(face="bold", color= "#0000FF", 
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#0000FF", 
                                   size=8, angle=0)) +
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="orange", size=14, face="bold"),
    axis.title.y = element_text(color="orange", size=14, face="bold")
  )

                  

# ****************************************************************************************************************


  
# A summary report for what kind of bikes are used by Casual & Member
 
ride_type <- time_length %>%                       # Generate a report
  group_by(month_year, member_casual, rideable_type) %>%
  summarise(count = sum(!is.na(rideable_type)))

ride_type_sorted <- arrange(ride_type, my(month_year))       # Sort it by Month/Year

# plot the data into graph
ggplot(data = ride_type_sorted) +
  geom_col(mapping = aes(x= my(month_year), y = count, fill = rideable_type)) +
  facet_grid(rideable_type~member_casual) +
  labs(x = "Month/Year", y = "Total Number of rides",  title = "Different Bike Type: Casual Riders VS. Members", 
       subtitle ="Total usage of different type of bikes are for last 12 months from 11/2020 to 10/2021") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y")) +
  theme(axis.text.x = element_text(face="bold", color= "#0000FF", 
                                   size=8, angle= 90),
        axis.text.y = element_text(face="bold", color="#0000FF", 
                                   size=8, angle=0)) +
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="orange", size=14, face="bold"),
    axis.title.y = element_text(color="orange", size=14, face="bold")
  )



# *******************************************************************************************************8

# A summary report for total number of rides for Casual Rider & Member

total_rides <- time_length %>%                       # Generate a report
  group_by(month_year, member_casual) %>%
  summarise(count = sum(!is.na(member_casual)))

total_rides_sorted <- arrange(total_rides, my(month_year))        #sort it by Month/Year

# plot the data into graph
ggplot(data = total_rides_sorted) +
  geom_col(mapping = aes(x= my(month_year), y = count, fill = member_casual), position = "dodge") +
  #facet_grid(~member_casual) +
  labs(x = "Month/Year", y = "Total Number of rides",  title = "Total Number of Rides Per Month: Casual Riders VS. Members", 
       subtitle ="Total number of rides are for last 12 months from 11/2020 to 10/2021") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y")) +
  scale_y_continuous(limits = c(0, 500000),
                     breaks=seq(0, 500000, 100000),
                     labels = comma) +
  theme(axis.text.x = element_text(face="bold", color= "#0000FF", 
                                   size=8, angle= 0),
        axis.text.y = element_text(face="bold", color="#0000FF", 
                                   size=8, angle=0)) +
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="orange", size=14, face="bold"),
    axis.title.y = element_text(color="orange", size=14, face="bold")
  )

  
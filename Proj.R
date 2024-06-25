install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")

library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)

setwd('C:/Users/rions/Desktop/My Projects/Cyclistic bike share analysis')

#Load the datasets using read.csv

jandf <- read.csv("./Data/202301-divvy-tripdata.csv")
febdf <- read.csv("./Data/202302-divvy-tripdata.csv")
mardf <- read.csv("./Data/202303-divvy-tripdata.csv")
aprdf <- read.csv("./Data/202304-divvy-tripdata.csv")
maydf <- read.csv("./Data/202305-divvy-tripdata.csv")
jundf <- read.csv("./Data/202306-divvy-tripdata.csv")
juldf <- read.csv("./Data/202307-divvy-tripdata.csv")
augdf <- read.csv("./Data/202308-divvy-tripdata.csv")
sepdf <- read.csv("./Data/202309-divvy-tripdata.csv")
octdf <- read.csv("./Data/202310-divvy-tripdata.csv")
novdf <- read.csv("./Data/202311-divvy-tripdata.csv")
decdf <- read.csv("./Data/202312-divvy-tripdata.csv")

#merge to single data frame

bikes_data <-rbind(jandf, febdf, mardf, aprdf, maydf, jundf, juldf, 
                   augdf, sepdf, octdf, novdf, decdf)

#remove all empty rows and cols

bikes_data <- remove_empty(bikes_data, which = c('cols', 'rows'))

summary(bikes_data)

#drop all null values

bikes_data <- bikes_data %>%
  drop_na()

summary(bikes_data)

#parse strings and convert them to Date/Time

bikes_data$started_at <- ymd_hms(bikes_data$started_at)
bikes_data$ended_at <- ymd_hms(bikes_data$ended_at)

#calculate trip duration of all instances in minutes

bikes_data <- bikes_data %>%
  mutate(trip_duration=as.numeric(difftime(ended_at, started_at, units="mins")))

#filter out negative trip durations

bikes_data <- bikes_data %>%
  filter(trip_duration > 0)

#calculate day of week and hour of day

bikes_data <- bikes_data %>%
  mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE), hour_of_day = hour(started_at))

head(bikes_data)

#trip duration summary to find mean, median, max and min

trip_duration_summary <- bikes_data %>%
  group_by(member_casual) %>%
  summarise(mean_trip_duration = mean(trip_duration),
            median_trip_duration = median(trip_duration),
            max_trip_duration = as.numeric(max(trip_duration)),
            min_trip_duration = as.numeric(min(trip_duration)))

print(trip_duration_summary)

#day of week summary to count the number of times bike has been rented in a day arranged in ascending order

day_of_week_summary <- bikes_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(trip_count = n()) %>%
  arrange(member_casual, day_of_week)

print(day_of_week_summary)

#hour of day summary to count the number of times bike has been rented in an hour arranged in ascending order

hour_of_day_summary <- bikes_data %>%
  group_by(member_casual, hour_of_day) %>%
  summarise(trip_count = n()) %>%
  arrange(member_casual, hour_of_day)

print(hour_of_day_summary, n=100)

# Bar plot for mean trip duration
mean_plot <- ggplot(trip_duration_summary, aes(x = member_casual, y = mean_trip_duration, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Trip Duration by User Type", y = "Mean Trip Duration (minutes)", x = "User Type") +
  theme_minimal()

# Bar plot for median trip duration
median_plot <- ggplot(trip_duration_summary, aes(x = member_casual, y = median_trip_duration, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Median Trip Duration by User Type", y = "Median Trip Duration (minutes)", x = "User Type") +
  theme_minimal()

# Arrange the plots in a grid
grid.arrange(mean_plot, median_plot)

# Plot trips by day of week
ggplot(day_of_week_summary, aes(x = day_of_week, y = trip_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels=comma) +
  labs(title = "Trips by Day of Week", y = "Number of Trips", x = "Day of Week")

# Plot trips by hour of day
ggplot(hour_of_day_summary, aes(x = hour_of_day, y = trip_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels=comma) +
  labs(title = "Trips by Hour of Day", y = "Number of Trips", x = "Hour of Day")
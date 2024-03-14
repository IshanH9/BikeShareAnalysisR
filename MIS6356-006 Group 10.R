# Load packages needed
library(tidyverse) # wrangle data
library(lubridate) # wrangle date attributes
library(ggplot2) # visualize data
library (dplyr)

#LOAD DATA
library(readxl)
novemberbike <- read_excel("Documents/bikesharing.xls")
View(novemberbike)
#nov <- read.csv('novemberbike.csv')
sample <- sample_n(novemberbike, 2000)
#CHECK FOR MISSING VALUES

missing_values <- any(is.na(sample))

if (missing_values) {
  # If there are missing values, remove rows with missing values
  sample <- na.omit(sample)
}


if (missing_values) {
  # If there are missing values, remove rows with missing values
  sample <- na.omit(sample)
}


# Check for missing values in the 'sample' data frame
if (any(is.na(sample))) {
  print("There are missing values in the 'sample' data frame.")
} else {
  print("There are no missing values in the 'sample' data frame.")
}


 sample <- mutate(sample, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
 
 # Remove all latitude and longitude
sample <- sample %>% 
   select(-c(start_lat, start_lng, end_lat, end_lng))

# Inspect the new table/COLUMNS created 
colnames(sample) # list of column names
nrow(sample) # how many roaws are in the data frame
dim(sample) # dimensions of the data frame
head(sample) # see the first 6 rows
str(sample) # see list of columns and data types
summary(sample) # statistical summary of the data (mainly for numeric data)

# How many observations fall under each rider type?
table(sample$member_casual)

# Add columns that list the date, month, day, and year of each ride
sample$date <- as.Date(sample$started_at) # the default format is yyyy-mm-dd
sample$month <- format(as.Date(sample$date), "%B")
sample$day <- format(as.Date(sample$date), "%d")
sample$year <- format(as.Date(sample$date), "%Y")
sample$day_of_week <- format(as.Date(sample$date), "%a")


# Calculate the "ride_length" and add a new column
sample$ride_length <- difftime(sample$ended_at, sample$started_at, units = "mins")

# Inspect the structure of the columns again
str(sample)

# Convert "ride_length" from Factor to numeric to further run calculations
is.factor(sample$ride_length)
sample$ride_length <- as.numeric(as.character(sample$ride_length))
is.numeric(sample$ride_length)

# Remove trips that the ride length is <= 0 or more than one day (24 * 60 = 1440 minutes)
# Make a copy for the cleaned data frame
sample_v2 <- sample[!(sample$ride_length > 1440 | sample$ride_length <= 0),]
str(sample_v2)

# Combine start and end stations)
# Removing entries with no station name
# Separate the data frame by rider type
all_stations <- bind_rows(data.frame("stations" = sample_v2$start_station_name, 
                                     "member_casual" = sample_v2$member_casual),
                          data.frame("stations" = sample_v2$end_station_name,
                                     "member_casual" = sample_v2$member_casual))
all_stations_v2 <- all_stations[!(all_stations$stations == "" | is.na(all_stations$stations)),]
all_stations_member <- all_stations_v2[all_stations_v2$member_casual == 'member',]
all_stations_casual <- all_stations_v2[all_stations_v2$member_casual == 'casual',]

# Get the top 10 popular stations all, members, and casual riders
top_10_station <- all_stations_v2 %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  slice(1:10)

top_10_station_member <- all_stations_member %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)

top_10_station_casual <- all_stations_casual %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)


# Top 20 start stations for casual riders
sample_v2 %>% 
  group_by(start_station_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(start_station_name != "", member_casual != "member") %>% 
  arrange(-number_of_rides) %>% 
  head(n=20)

# Approach one
summary(sample_v2$ride_length)

# Approach two
sample_v2 %>% 
  summarise(min_ride_length = min(ride_length), 
            max_ride_length = max(ride_length),
            median_ride_length = median(ride_length),
            mean_ride_length = mean(ride_length))

# Compare ride length between members and casual riders
aggregate(sample_v2$ride_length ~ sample_v2$member_casual, FUN = mean)
aggregate(sample_v2$ride_length ~ sample_v2$member_casual, FUN = median)
aggregate(sample_v2$ride_length ~ sample_v2$member_casual, FUN = max)
aggregate(sample_v2$ride_length ~ sample_v2$member_casual, FUN = min)




# See the average ride length by each day of week for members vs. casual riders
aggregate(sample_v2$ride_length ~ sample_v2$member_casual + sample_v2$day_of_week, FUN = mean)



# Put days of the week in order
sample_v2$day_of_week <- ordered(sample_v2$day_of_week, 
                                    levels = c("Mon", 
                                               "Tue", "Wed",
                                               "Thu", "Fri", "Sat", "Sun"))

# Again, see the average ride length by each day of week for members vs. casual riders
aggregate(sample_v2$ride_length ~ sample_v2$member_casual + sample_v2$day_of_week, FUN = mean)


# Median ride length between members and casual riders for each day of week
aggregate(sample_v2$ride_length ~ sample_v2$member_casual + sample_v2$day_of_week, FUN = median)



# Number of rides between members and casual riders for each day of week
sample_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(day_of_week)

 
 
# Comparing general bike type preference between members and casual riders
sample_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop')

# Comparing number of docked_bike rides between members and casual riders for each day of week
sample_v2 %>% 
  filter(rideable_type == 'docked_bike') %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(day_of_week)

# Docked_bike rides of casual riders for each day of week
sample_v2 %>% 
  filter(rideable_type == 'docked_bike', member_casual == 'casual') %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop')



my_theme = theme(plot.title=element_text(size=20),
                 axis.text.x=element_text(size=16), 
                 axis.text.y=element_text(size=16),
                 axis.title.x=element_text(size=18), 
                 axis.title.y=element_text(size=18),
                 strip.text.x=element_text(size=16), 
                 strip.text.y=element_text(size=16),
                 legend.title=element_text(size=18), 
                 legend.text=element_text(size=16))

options(repr.plot.width = 6, repr.plot.height = 8)

sample_v2 %>% 
  group_by(member_casual) %>% 
  summarize(average_duration = mean(ride_length)) %>% 
  ggplot(aes(x = member_casual, y = average_duration)) +
  geom_col(position = "dodge") +
  labs(x = "Rider Type", y = "Average Duration (min)", 
       title = "Average Riding Duration by Rider Type") + my_theme




options(repr.plot.width = 10, repr.plot.height = 8)

sample_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_duration = mean(ride_length), .groups = 'drop') %>% 
  #arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Day of Week", y = "Average Duration (min)", 
       fill = "Member/Casual",
       title = "Average Riding Duration by Day: Members vs. Casual Riders") + my_theme


str(sample_v2)
data <-sample_v2[, sapply(sample_v2, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(data)

# Load the corrplot library
library(corrplot)

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", tl.srt = 45)



sample_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  #arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Day: Members vs. Casual Riders") + my_theme




options(repr.plot.width = 10, repr.plot.height = 8)
 


# Compare number of rides in different months
sample_v2  %>% 
  group_by(member_casual,month=month(started_at,label=TRUE)) %>%
  summarise(num_rides = n()/1000) %>%
  ggplot(aes(x=month, y=num_rides, fill=member_casual)) +
  geom_col(position="dodge") +  
  labs(title = "Ride count by month", x = "Month", y = "Ride count (Thousands)")


str(sample_v2)
sample_v2$started_at_hour <- as.POSIXct(sample_v2$started_at, "%Y-%m-%d %H:%M:%S")      
str(sample_v2)

sample_v2$started_at_hour <- as.POSIXct(sample_v2$started_at, format = "%Y-%m-%d %H:%M:%S")


options(repr.plot.width = 12, repr.plot.height = 8)

sample_v2 %>%
  filter(member_casual == 'casual') %>%
  group_by(hour_of_day = hour(round_date(started_at_hour, 'hour'))) %>% 
  group_by(hour_of_day, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(-number_of_rides) %>% 
  ggplot(aes(x = hour_of_day, y = number_of_rides, fill = member_casual)) +
  geom_bar(position = 'dodge', stat = 'identity') + scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  labs(x = "Time of the Day (h)", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Average Number of Rides by Hour: Casual Riders") + my_theme





options(repr.plot.width = 10, repr.plot.height = 6)

ggplot(data = top_10_station_member) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = "thistle") +
  labs(title = "Top 10 Used Stations by Members", y = "Number of Rides", x = "") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal() + my_theme


ggplot(data = top_10_station_casual) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = "lightsalmon") +
  labs(title = "Top 10 Used Stations by Casual Riders", x = "", y = "Number of Rides") + 
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal() + my_theme


options(repr.plot.width = 10, repr.plot.height = 6)

sample_v2 %>% 
  group_by(start_station_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(start_station_name != "", member_casual != "member") %>% 
  arrange(-number_of_rides) %>% 
  head(n=10) %>%
  ggplot(aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides)) +
  geom_col(position = 'dodge', fill = '#f8766d') +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Top 10 Start Stations for Casual Riders', x = '', y = "Number of Rides") +
  coord_flip() +
  theme_minimal() +
  my_theme


options(repr.plot.width = 12, repr.plot.height = 8)

sample_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = member_casual, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~rideable_type) +
  labs(fill = "Member/Casual", x = "", y = "Number of Rides", 
       title = "Usage of Different Bikes: Members vs. Casual Riders") + my_theme

sample_v2 <- sample[!(sample$ride_length > 1440 | sample$ride_length <= 0),]
str(sample_v2)


# Load required packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(rpart)  # for decision tree modeling
library(neuralnet)
install.packages("caret")

library(caret)

sample_v2$started_at_hour <- as.POSIXct(sample_v2$started_at, format = "%Y-%m-%d %H:%M:%S")

# Convert member_casual to a factor
sample_v2$member_casual <- as.factor(sample_v2$member_casual)

# Check unique values in member_casual before conversion
unique(sample_v2$member_casual)

# Convert factor levels to numeric with meaningful mapping
sample_v2$member_casual <- as.numeric(factor(sample_v2$member_casual, levels = c("member", "casual")))

# Check unique values in member_casual after conversion
unique(sample_v2$member_casual)


# Convert categorical variables to factors


# Check unique values in day_of_week before conversion
unique(sample_v2$day_of_week)

# Convert day_of_week to a factor
sample_v2$day_of_week <- as.factor(sample_v2$day_of_week)

# Check unique values in day_of_week before numeric conversion
unique(sample_v2$day_of_week)

# Convert factor levels to numeric with meaningful mapping
sample_v2$day_of_week <- as.numeric(factor(sample_v2$day_of_week, levels = unique(sample_v2$day_of_week)))

# Check unique values in day_of_week after conversion
unique(sample_v2$day_of_week)
 

# Check unique values in day_of_week before conversion
unique(sample_v2$rideable_type)

# Convert day_of_week to a factor
sample_v2$rideable_type <- as.factor(sample_v2$rideable_type)

# Check unique values in day_of_week before numeric conversion
unique(sample_v2$rideable_type)

# Convert factor levels to numeric with meaningful mapping
sample_v2$rideable_type <- as.numeric(factor(sample_v2$rideable_type, levels = unique(sample_v2$rideable_type)))

# Check unique values in day_of_week after conversion
unique(sample_v2$rideable_type)

any(is.na(sample_v2$member_casual))

#  Remove rows with missing values
sample_v2 <- na.omit(sample_v2)
 
# Create a training set and a testing set
set.seed(123)
split_index <- createDataPartition(sample_v2$member_casual, p = 0.7, list = FALSE)
train_data <- sample_v2[split_index, ]
test_data <- sample_v2[-split_index, ]

# Assuming 'member_casual' is the target variable
target_variable <- "member_casual"

# Remove the target variable from the test data
test_data_without_target <- subset(test_data, select = -c(member_casual))
unique_values <- unique(sample_v2$member_casual)
length(unique_values)


# Remove rows with missing values
train_data <- na.omit(train_data)

if (any(is.na(train_data))) {
  stop("Training data contains missing values. Please handle or remove them.")
}


train_data$started_at_hour_scaled <- scale(train_data$started_at_hour)
test_data$started_at_hour_scaled <- scale(test_data$started_at_hour)
test_data_without_target$started_at_hour_scaled<- scale(test_data$started_at_hour)
# Train a decision tree model with the scaled variable
tree_model <- rpart(member_casual ~ ride_length + day_of_week + started_at_hour_scaled + rideable_type,
                    data = train_data, method = "class")

# Make predictions on the test set
predictions <- predict(tree_model, newdata = test_data_without_target, type = "class")
View(test_data_without_target)
# Evaluate model accuracy
confusion_matrix <- table(predictions, test_data$member_casual)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))

# Visualize the decision tree
rpart.plot(tree_model, box.palette = "auto", shadow.col = "gray", nn = TRUE)

 

 






 
  
  
  
  
  # Create a formula for the neural network
  formula <- as.formula(member_casual ~ ride_length + day_of_week +started_at_hour_scaled+ rideable_type)
  
  # Train a neural network model
  
  nn_model <- neuralnet( formula, data = train_data,linear.output = F, hidden = 5,learningrate=1.5)
  
   
 
 plot(nn_model, rep="best")
   
  
  # Make predictions on the test set
  nn_pred <- predict(nn_model, newdata = test_data)
  
  # Convert probabilities to classes (binary classification)
  nn_pred_classes <- ifelse(nn_pred > 0.5, 1, 0)
  
  # Create a confusion matrix
  conf_matrix <- table(nn_pred_classes, test_data$member_casual)
  
  # Print the confusion matrix
  print("Confusion Matrix:")
  print(conf_matrix)
  
  # Calculate accuracy
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  print(paste("Accuracy: ", round(accuracy * 100, 2), "%"))
   
  
  
  
  
  
   
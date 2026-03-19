setwd("E:/新技能/Epidemiology/John_Hopkins_Reproducible_research/RepData_PeerAssessment1")

unzip("activity.zip", 
      exdir = "E:/新技能/Epidemiology/John_Hopkins_Reproducible_research/RepData_PeerAssessment1")
activity <- read.csv("activity.csv")

# Calculated the total number of steps per day
total_steps <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)

# Make a histogram for the total number of steps per day
hist(total_steps$steps,
     breaks = 20,
     main = "Total Number of Steps Per Day", 
     col = "orange", 
     xlab = "Total Number of Steps (per day)", 
     ylab = "Number of days")


# Calculated the average number of steps per day
average_steps <- aggregate(steps ~ date, data = activity, FUN = mean, na.rm = TRUE)
average_steps

# Calculated the median number of steps per day
median_steps <- aggregate(steps ~ date, data = activity, FUN = median, na.rm = TRUE)
median_steps

# Time series plot of the average number of steps taken per day
library(ggplot2)
ggplot(average_steps,aes(x = date, y = steps, group = 1)) + 
  # group = 1 means that all points will be regarded as a group
  geom_point(color = "darkblue") + # Add points
  geom_line(color = "darkblue") +  # Add line
  labs(x = "Date",
       y = "Average number of steps (per day)",
       title = "Average Steps by Date") +
  theme_bw() + 
  # Add the theme before adjust the label for x-axis
  # or it will go to default setting (be horizontal)
  theme(axis.text.x = element_text(angle = 45,  # 45-degree rotation
                                   hjust = 1)) # horizontal justification

dev.off() 

# Find out the 5-minute interval that contains the maximum average steps
interval_avg <- aggregate(steps ~ interval, data = activity, FUN = mean)
max_interval <- interval_avg[which.max(interval_avg$steps),]
max_interval 

###### Code to describe and show a strategy for imputing missing data ######

# Install necessary libraries (if not already installed)
# install.packages("mice")
# install.packages("mitools")
# install.packages("VIM")

# Load necessary packages
library(mice)  # For multiple imputation
library(mitools)  # For pooling multiple imputation results
library(VIM)  # For KNN imputation

# Ensure 'date' column is in Date format
activity$date <- as.Date(activity$date)

# Perform multiple imputation using PMM(Predictive Mean Matching) method
# And PMM will generate 12 imputed datasets as we set m = 12
imp <- mice(activity, m = 12, method = 'pmm')

# Retrieve all 12 imputed datasets and store them in a list
imputed_data_list <- lapply(1:12, function(i) complete(imp, i))

# Perform regression analysis (e.g., linear regression) on each imputed dataset
pooled_results <- with(imp, lm(steps ~ interval * factor(date)))

# Pool the results from all imputed datasets
pooled_summary <- pool(pooled_results)

# View the summary of the pooled regression analysis results
summary(pooled_summary)

# Calculate total steps per day for all 12 imputed datasets
total_steps_list <- lapply(1:12, function(i) {
  data_imputed <- complete(imp, i)  # Retrieve each imputed dataset
  aggregate(steps ~ date, data = data_imputed, FUN = sum, na.rm = TRUE)  
  # Calculate total steps per day
})

# Combine all the imputed datasets' total steps into one data frame
total_steps_all <- do.call(rbind, total_steps_list)

# Plot histograms to compare the distribution of data before and after imputation
par(mfrow = c(1, 3))  # Split the plot window into 1 row and 3 columns

# Histogram before imputation (original data)
hist(activity$steps, 
     main = "Total Steps Taken Each Day Across Original Dataset",  # Title for the histogram before imputation
     col = "lightblue",  # Color of the histogram bars
     xlab = "Total Steps Per Day",  # X-axis label
     ylab = "Number of Days",  # Y-axis label
     breaks = 20)  # Set the number of bins for the histogram

# Histogram after imputation 
# Histogram after imputation using PMM method
# Plot a histogram of the total steps taken each day across all imputed datasets
hist(total_steps_all$steps, 
     main = "Total Steps Taken Each Day Across Pooled Imputed Datasets (PMM)",  # Title for the histogram
     col = "lightgreen",  # Color of the histogram bars
     xlab = "Total Steps Per Day",  # X-axis label
     ylab = "Number of Days",  # Y-axis label
     breaks = 20)  # Set the number of bins for the histogram

# Use knn(K-Nearest Neighbors) multiple imputation method for imputing missing data 
imputed_data_knn <- kNN(activity, k = 5)  # Set the number of nearest neighbors equal to 5

# Calculate total steps per day for KNN-imputed data
knn_imputed_data_sum <- aggregate(steps ~ date, data = imputed_data_knn, FUN = sum, na.rm = TRUE)

# Histogram after imputation using knn method
hist(knn_imputed_data_sum$steps,
     main = "Total Steps Taken Each Day in KNN Imputed Dataset",  # Title of the plot
     col = "lightskyblue",  # Color of the bars
     xlab = "Total Steps Per Day",  # X-axis label
     ylab = "Number of Days",  # Y-axis label
     breaks = 20)  # Set the number of bins for the histogram

# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

# Set the language in English 
# Make sure the locale setting works as expected
Sys.setlocale("LC_ALL", "English_United States.UTF-8")  

# Load the package for processing date
library(lubridate)

# Convert the date column to date format and create a new 'weekdays' column
activity$weekdays <- weekdays(as.Date(activity$date))

# Create a new column 'group' based on weekdays
activity$group <- ifelse(activity$weekdays 
                         %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                         "weekday", "weekend")

# Load the ggplot2 package for plotting
library(ggplot2)

# Generate the plot
ggplot(activity, aes(x = interval, y = steps)) + 
  # Calculate the mean of 'steps' for each 'interval', ignoring NAs, and plot as a line
  stat_summary(fun = "mean", na.rm = TRUE, geom = "line") + 
  facet_grid(. ~ group) + # Split the plot into panels based on the 'group' variable
  labs(x = "Time (5-min intervals)", y = "Average Steps") + # Set labels for the x-axis and y-axis
  theme_bw()  # Apply the black and white theme to the plot

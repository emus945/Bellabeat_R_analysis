install.packages("tidyverse")
library("tidyverse")
install.packages("lubridate")
library("lubridate")
library("tidyr")
library("dplyr")
install.packages("skimr")
library("skimr")
install.packages("gglot2")
library(ggplot2)
install.packages("viridis")
library(viridis)
str(dailyActivity_merged)
str(dailyCalories_merged)
str(dailyIntensities_merged)
str(dailySteps_merged)
str(sleepDay_merged)
#Cleaning Process
dailyActivity <- dailyActivity_merged %>%
  distinct() %>%
  drop_na() %>%
  rename(date = ActivityDate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))
summary(dailyActivity)
#Calories
dailyCalories <- dailyCalories_merged %>%
  distinct() %>%
  drop_na() %>%
  rename(date = ActivityDay) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))
summary(dailyCalories)
#Sleep
sleep <- sleepDay_merged %>%
  distinct() %>%
  drop_na() %>%
  rename(date = SleepDay) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p"))
summary(sleep)
#Steps
daySteps <- dailySteps_merged %>%
  distinct() %>%
  drop_na() %>%
  rename(date = ActivityDay) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))
summary(daySteps)
#Intensity
dailyIntensity <-dailyIntensities_merged %>%
  distinct() %>%
  drop_na() %>%
  rename(date = ActivityDay) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))
summary(dailyIntensity)
#combine the tables by (id, date)
merged_data <- merge(dailyActivity, daySteps, by = c("Id", "date"))
merged_data <- merge(merged_data, sleep, by = c("Id", "date"))
merged_data <- merge(merged_data, dailyIntensity, by = c("Id", "date"))
merged_data <- merge(merged_data, dailyCalories, by = c("Id", "date"))

View(merged_data)

write.table(merged_data,"merged_data1.csv", row.names =FALSE, sep = ",")

min_date <- min(merged_data$date)
max_date <- max(merged_data$date)

merged_data$Id <- as.factor(merged_data$Id)
ggplot(data = merged_data, mapping = aes(x = StepTotal, y = TotalDistance)) + geom_point() +
  geom_smooth() + labs(title="Steps and Distance Activity",
                       caption=paste0("Data from: ", min_date, " to ", max_date)) + scale_color_discrete(name = "Id", labels = levels(merged_data$Id))


ggplot(data = merged_data, mapping = aes(x = StepTotal, fill = ..count..)) +
  geom_histogram() +
  labs(title = "Distribution(Steps Totals)",
       x = "Step Total",
       y = "Count",
       caption = paste0("Data from: ", min_date, " to ", max_date)) +
  scale_fill_gradient(low = "red", high = "yellow", guide = "legend") +
  theme_minimal()

ggplot(data = merged_data, mapping = aes(x = TotalTimeInBed, y = TotalMinutesAsleep)) +
  geom_smooth() +
  geom_point() +
  labs(title="Total Time in Bed vs Minutes Asleep",
       caption=paste0("Data from: ", min_date, " to ", max_date)) + scale_color_discrete(name = "Id", labels = levels(merged_data$Id))


merged_data2 <- merged_data %>%
  gather(key = "activity_type", value = "activity_minutes",
         SedentaryMinutes.x : VeryActiveMinutes.x)
ggplot(data = merged_data2, aes(x = activity_minutes, fill = activity_type)) +
  geom_histogram(alpha = 0.5, binwidth = 20) +
  labs(title = "Distribution(Activity Levels)", subtitle = "Elvis Musonera (Bellaabeat)",
       x = "Activity Minutes",
       y = "Count") +
  theme_minimal()


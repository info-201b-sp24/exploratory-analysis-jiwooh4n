x_values <- seq(1, 3)
y_values <- seq(1,3)

library(ggplot2)
ggplot() +
  geom_point(aes(x=x_values, y = y_values))



# -------------------- Loading dataset --------------------

# load the CSV files

sleepdata <- read.csv("./data/Sleep_Efficiency.csv")
lifestyle <- read.csv("./data/Sleep_health_and_lifestyle_dataset.csv")


# -------------------- Cleaning dataset --------------------

# Removing missing values and handling inconsistencies

sleepdata <- na.omit(sleepdata)
lifestyle <- na.omit(lifestyle)

# Selecting only neccessary columns for each dataset

sleepdata <- select(sleepdata, Age, Gender, Sleep.duration, REM.sleep.percentage, Caffeine.consumption)
lifestyle <- select(lifestyle, Gender, Age, Occupation, Sleep.Duration, Quality.of.Sleep, Stress.Level)


# -------------------- Aggregated and visualized sleepdata --------------------

# Extracting values and visualizing for summary - sleepdata

#1 value
avg_age_sleepdata <- sleepdata %>%
  group_by(Age.range = cut(Age, breaks = seq(0, 70, by = 10), include.lowest = TRUE, right = FALSE, labels = c("0-10", "11-20", "21- 30", "31-40", "41-50", "51-60", "61-70"))) %>%
  summarise(Avg_Sleep_Duration = mean(Sleep.duration, na.rm = TRUE))
  
#1 Visualization
ggplot(avg_age_sleepdata, aes(x = Age.range, y = Avg_Sleep_Duration, fill = Age.range)) + 
  geom_bar(stat = "identity") +
  labs(title = "Average Sleep Duration by Age Range", x = "Age Range", y = "Average Sleep Duration (hours)")


#2 value
avg_sleep_rem_by_gender <- sleepdata %>%
  group_by(Gender) %>%
  summarise(
    Avg_Gender_Sleep_Duration = mean(Sleep.duration, na.rm = TRUE),
    Avg_REM_Sleep_Percentage = mean(REM.sleep.percentage, na.rm = TRUE)
  )

#2-1 visualization
ggplot(avg_sleep_rem_by_gender, aes(x = Gender, y = Avg_Gender_Sleep_Duration, group = 1, color = Gender)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Average Sleep Duration by Gender", x = "Gender", y = "Average Sleep Duration (hours)")

#2-2 visualization
ggplot(avg_sleep_rem_by_gender, aes(x = Gender, y = Avg_REM_Sleep_Percentage, color = Gender)) +
  geom_point(size = 5) +
  labs(title = "Average REM Sleep Percentage by Gender", x = "Gender", y = "Average REM Sleep Percentage")

#3 value
filtered_data <- sleepdata %>%
  mutate(Caffeine.range = cut(Caffeine.consumption, 
                              breaks = c(-1, 0, 25, 50, 75, 200),
                              labels = c("0", "1-25", "26-50", "51-75", "76-200")))

avg_sleep_by_caffeine <- filtered_data %>%
  group_by(Caffeine.range) %>%
  summarise(Avg_Sleep_Duration = mean(Sleep.duration, na.rm = TRUE))

#3 visualization
ggplot(avg_sleep_by_caffeine, aes(x = Caffeine.range, y = Avg_Sleep_Duration, fill = Caffeine.range)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Sleep Duration by Caffeine Consumption Range", x = "Caffeine Consumption Range (mg)", y = "Average Sleep Duration (hours)")


# Extracting values and visualizing for summary - lifestyle

#4 value
avg_sleep_duration_by_occupation <- lifestyle %>%
  group_by(Occupation) %>%
  summarise(Avg_Sleep_Duration = mean(Sleep.Duration, na.rm = TRUE))

#4 visualization
ggplot(avg_sleep_duration_by_occupation, aes(x = reorder(Occupation, Avg_Sleep_Duration), y = Avg_Sleep_Duration, fill = Occupation)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Sleep Duration by Occupation", x = "Occupation", y = "Average Sleep Duration (hours)")

#5 value
avg_quality_of_sleep_by_stress_level <- lifestyle %>%
  group_by(Stress.Level) %>%
  summarise(Avg_Quality_of_Sleep = mean(Quality.of.Sleep, na.rm = TRUE)) %>%
  arrange(Stress.Level)

#5 visualization
ggplot(avg_quality_of_sleep_by_stress_level, aes(x = Stress.Level, y = Avg_Quality_of_Sleep)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "Average Quality of Sleep by Stress Level", x = "Stress Level", y = "Average Quality of Sleep")












colnames(dataset1)[2] <- "Sleep.duration"

merged_sleep <- merge(sleepdata, lifestyle, by = "Gender")

merged_sleep_duration <- merge(merged_sleep, dataset1, by = "Sleep.duration")


# -------------------- Categorizing dataset --------------------

merged_age <- merge(sleepdata, lifestyle, by = "Age")

# -------------------- Age range dataset --------------------

age_divided <- c(20, 26, 31, 36, 41, 46, 51, 56, 61)
age_labeled <- c("20-25", "26-30", "31-35", "36-40", "41-45", 
                 "46-50", "51-55", "56-60")

sleepdata <- sleepdata %>%
  mutate(Age_Range = cut(Age, breaks = age_divided, labels = age_labeled, right = FALSE))

lifestyle <- sleepdata %>%
  mutate(Age_range = cut(Age, breaks = age_divided, labels = age_labeled, right = FALSE))

categorize_age <- merge(sleepdata, lifestyle, by = "Age_Range")


# -------------------- Caffeine consumption dataset --------------------
caffeine_consumption <- merge(sleepdata, lifestyle, by = "Caffeine.consumption")

#-------------------------------Charting---------------------------------
library(ggplot2)
?ggplot

# Caffeine consumption vs Sleep duration
colnames(merged_sleep)

merged_sleep$Caffeine.consumption <- as.numeric(as.character(merged_sleep$Caffeine.consumption))
merged_sleep$Sleep.duration <- as.numeric(as.character(merged_sleep$Sleep.duration))

ggplot(merged_sleep, aes(x = Caffeine.consumption, y = Sleep.duration)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Caffeine Consumption vs Sleep Duration",
       x = "Caffeine Consumption (mg)",
       y = "Sleep Duration (hours)") +
  theme_minimal()


# Sleep Duration vs Physical Activity Level

merged_sleep_duration$Sleep.duration <- as.numeric(as.character(merged_sleep_duration$Sleep.duration))
merged_sleep_duration$Physical.Activity.Level <- as.numeric(as.character(merged_sleep_duration$Physical.Activity.Level))

ggplot(merged_sleep_duration, aes(x = Sleep.duration, y = Physical.Activity.Level)) +
  geom_line(color = "blue") +
  labs(title = "Sleep Duration vs. Physical Activity",
       x = "Sleep Duration",
       y = "Physical Activity") +
  theme_minimal()
  
# REM sleep percentage vs Caffeine consumption
  
merged_sleep$Caffeine.consumption <- as.numeric(as.character(merged_sleep$Caffeine.consumption))
merged_sleep$REM.sleep.percentage <- as.numeric(as.character(merged_sleep$REM.sleep.percentage))
  
aggregated_data <- merged_sleep %>%
  group_by(Caffeine.consumption) %>%
  summarize(mean_REM = mean(REM.sleep.percentage, na.rm = TRUE))

ggplot(aggregated_data, aes(x = Caffeine.consumption, y = mean_REM)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Mean REM Sleep Percentage vs Caffeine Consumption",
       x = "Caffeine Consumption (mg)",
       y = "Mean REM Sleep Percentage (%)")


# Sleep Duration vs Stress Level (aggregated data)

merged_sleep_duration$Sleep.duration <- as.numeric(as.character(merged_sleep_duration$Sleep.duration))
merged_sleep_duration$Stress.Level.x <- as.numeric(as.character(merged_sleep_duration$Stress.Level.x))

aggregated_data_sleep <- merged_sleep_duration %>%
  group_by(Stress.Level.x) %>%
  summarize(mean_sleep_duration = mean(Sleep.duration, na.rm = TRUE))

ggplot(aggregated_data_sleep, aes(x = Stress.Level.x, y = mean_sleep_duration)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Sleep Duration vs Stress Level (aggregated data)",
       x = "Stress Level",
       y = "Mean Sleep Duration")


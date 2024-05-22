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
avg_age_sleepdata_chart <- ggplot(avg_age_sleepdata, aes(x = Age.range, y = Avg_Sleep_Duration, fill = Age.range)) + 
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
avg_sleep_duration_by_gender_chart <- ggplot(avg_sleep_rem_by_gender, aes(x = Gender, y = Avg_Gender_Sleep_Duration, group = 1, color = Gender)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Average Sleep Duration by Gender", x = "Gender", y = "Average Sleep Duration (hours)")

#2-2 visualization
avg_sleep_rem_by_gender_chart <- ggplot(avg_sleep_rem_by_gender, aes(x = Gender, y = Avg_REM_Sleep_Percentage, color = Gender)) +
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
avg_sleep_by_caffeine_chart <- ggplot(avg_sleep_by_caffeine, aes(x = Caffeine.range, y = Avg_Sleep_Duration, fill = Caffeine.range)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Sleep Duration by Caffeine Consumption Range", x = "Caffeine Consumption Range (mg)", y = "Average Sleep Duration (hours)")


# Extracting values and visualizing for summary - lifestyle

#4 value
avg_sleep_duration_by_occupation <- lifestyle %>%
  group_by(Occupation) %>%
  summarise(Avg_Sleep_Duration = mean(Sleep.Duration, na.rm = TRUE))

#4 visualization
avg_sleep_duration_by_occupation_chart <- ggplot(avg_sleep_duration_by_occupation, aes(x = reorder(Occupation, Avg_Sleep_Duration), y = Avg_Sleep_Duration, fill = Occupation)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Sleep Duration by Occupation", x = "Occupation", y = "Average Sleep Duration (hours)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#5 value
avg_quality_of_sleep_by_stress_level <- lifestyle %>%
  group_by(Stress.Level) %>%
  summarise(Avg_Quality_of_Sleep = mean(Quality.of.Sleep, na.rm = TRUE)) %>%
  arrange(Stress.Level)

#5 visualization
avg_quality_of_sleep_by_stress_level_chart <- ggplot(avg_quality_of_sleep_by_stress_level, aes(x = Stress.Level, y = Avg_Quality_of_Sleep)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "Average Quality of Sleep by Stress Level", x = "Stress Level", y = "Average Quality of Sleep")


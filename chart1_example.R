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

# Selecting only neccessary columns for sleepdata

sleepdata <- select(sleepdata, Age, Gender, Sleep.duration, REM.sleep.percentage, Caffeine.consumption)

# Selecting only neccessary columns for lifestyle
# -------------------- Merging dataset --------------------

merged_sleep <- merge(sleepdata, lifestyle, by = "Gender")


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
       y = "Sleep Duration (hours)") 


# REM sleep percentage vs Age

merged_sleep <- merged_sleep %>%
  filter(!is.na(Age.x) & !is.na(REM.sleep.percentage))

merged_sleep$Age.x <- as.numeric(as.character(merged_sleep$Age.x))
merged_sleep$REM.sleep.percentage <- as.numeric(as.character(merged_sleep$REM.sleep.percentage))

ggplot(merged_sleep, aes(x = Age.x, y = REM.sleep.percentage))+
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "REM Sleep Percentage vs. Age",
       x = "Age",
       y = "REM Sleep Percentage")
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
  
# loading another data file 

dataset1 <- read.csv("dataset1.csv")

# cleaning the data

dataset1 <- na.omit(dataset1)

# selecting only the necessary info

dataset1 <- select(dataset1, Age, Sleep.Duration, Quality.of.Sleep, Stress.Level)


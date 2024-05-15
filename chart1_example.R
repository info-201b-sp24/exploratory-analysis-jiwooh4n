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


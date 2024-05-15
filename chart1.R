# hello 
library(dplyr)

# loading my data file 

dataset1 <- read.csv("dataset1.csv")

# cleaning my data

dataset1 <- na.omit(dataset1)

# selecting only necessary info

dataset1 <- select(dataset1, Age, Sleep.Duration, Quality.of.Sleep, Stress.Level)

# merging this dataset with dataset2 

#  range 


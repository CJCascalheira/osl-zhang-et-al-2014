####### SETUP WORKSPACE #######

# Load dependencies
library(psych)
library(broom)
library(car)
library(nlme)
library(multcomp)
library(tidyverse)

# Set working directory
setwd("~/GitHub/osl-zhang-et-al-2014/src")

# Import data
zhang <- read_csv("../data/Zhang et al. 2014 Study 3.csv")

####### CLEAN DATA #######

# Dataset structure
glimpse(zhang)

# Select and rename variables of interest
zhang_clean <- zhang %>%
  select(
    condition = Condition,
    t1_extra = T1_Extraordinariness,
    t2_extra = T2_Extraordinariness,
    t1_curious = T1_Predicted_Curious,
    t2_curious = T2_Actual_Curious,
    t1_interest = T1_Predicted_Interest_Composite,
    t2_interest = T2_Actual_Interest_Composite
  )

# Condition as factor
zhang_clean <- within(zhang_clean, {
  condition <- factor(condition, labels = c("ordinary", "extraordinary"))
})

# Number of participants in each condition
zhang_clean %>% count(condition)

####### EXTRAORDINARINESS MANIPULATION CHECK #######

# Create analysis of variance object
t1_extra_aov <- aov(t1_extra ~ condition, data = zhang_clean)

# Summarize one-way ANOVA
Anova(t1_extra_aov)



####################################################################################
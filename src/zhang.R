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
  ) %>%
  mutate(subject_id = seq(1, 130, 1))

# Condition as factor
zhang_clean <- within(zhang_clean, {
  condition <- factor(condition, labels = c("ordinary", "extraordinary"))
  subject_id <- factor(subject_id)
})

# Number of participants in each condition
zhang_clean %>% count(condition)

# Split data by condition for descriptive statistics
ordinary <- zhang_clean %>% filter(condition == "ordinary")
extraordinary <- zhang_clean %>% filter(condition == "extraordinary")

# Convert data frame into long format
long_extra <- zhang_clean %>%
  select(condition, subject_id, t1_extra, t2_extra) %>%
  gather(key = extra_time, value = extra_rating, -c(condition, subject_id))

long_curious <- zhang_clean %>%
  select(t1_curious, t2_curious) %>%
  gather(key = curious_time, value = curious_rating)

long_interest <- zhang_clean %>%
  select(t1_interest, t2_interest) %>%
  gather(key = interest_time, value = interest_rating)

(zhang_long <- bind_cols(long_extra, long_curious, long_interest))

# Set time variables as factors
(zhang_long <- within(zhang_long, {
  extra_time <- factor(extra_time)
  curious_time <- factor(curious_time)
  interest_time <- factor(interest_time)
}))

####### EXTRAORDINARINESS MANIPULATION CHECK #######

# Create analysis of variance object
t1_extra_aov <- aov(t1_extra ~ condition, data = zhang_clean)

# Summarize one-way ANOVA
Anova(t1_extra_aov)

# Descriptive statistics
zhang_clean %>%
  group_by(condition) %>%
  summarize(
    mean = mean(t1_extra),
    sd = sd(t1_extra),
    n = n(),
    upper = mean - (1.96 * (sd / sqrt(n))),
    lower = mean + (1.96 * (sd / sqrt(n)))
  )

# Tidy aov object
(t1_extra_tidied <- tidy(t1_extra_aov))

# Partial eta-squared
(t1_extra_tidied[1, 3] / (t1_extra_tidied[1, 3] + t1_extra_tidied[2, 3])) %>%
  pull()

#### Outliers?
ggplot(zhang_clean, aes(x = condition, y = t1_extra)) +
  geom_boxplot()

#### Normality?
shapiro.test(residuals(t1_extra_aov))

#### Homoscedasticity?
leveneTest(t1_extra ~ condition, data = zhang_clean)

####################################################################################

####### EXTRAORDINARINESS OVER TIME BY CONDITION #######



#### Outliers?

#### Normality?

#### Homoscedasticity?

#### Sphericity?

####################################################################################

####### CURIOSITY OVER TIME BY CONDITION #######

#### Outliers?

#### Normality?

#### Homoscedasticity?

#### Sphericity?

####################################################################################

####### INTEREST OVER TIME BY CONDITION #######

#### Outliers?

#### Normality?

#### Homoscedasticity?

#### Sphericity?

####################################################################################

####### VISUALIZE #######

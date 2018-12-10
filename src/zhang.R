####### SETUP WORKSPACE #######

# Load dependencies
library(psych)
library(broom)
library(car)
library(nlme)
library(ez)
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

# Split data by condition
ordinary <- zhang_long %>% filter(condition == "ordinary")
extraordinary <- zhang_long %>% filter(condition == "extraordinary")

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
    lower = mean - (1.96 * (sd / sqrt(n))),
    upper = mean + (1.96 * (sd / sqrt(n)))
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

# Create analysis of variance
extra_aov <- aov(extra_rating ~ condition*extra_time + Error(subject_id), data = zhang_long)

# Summarize the ANOVA
summary(extra_aov)

# Tidy output
(extra_tidied <- tidy(extra_aov))

# Partial eta-squared for main effect of time
extra_tidied$sumsq[3] / (extra_tidied$sumsq[3] + extra_tidied$sumsq[5])

# Partial eta-squared for interaction between time and condition
extra_tidied$sumsq[4] / (extra_tidied$sumsq[4] + extra_tidied$sumsq[5])

# Create linear mixed-effects model
extra_lme <- lme(extra_rating ~ condition*extra_time, random = ~1|subject_id,
                 data = zhang_long)

# Print ANOVA summary using type III sum of squares
options(contrasts = c("contr.sum", "contr.poly"))
Anova(extra_lme, type = "III")

# Descriptives for time
zhang_long %>%
  group_by(extra_time) %>%
  summarize(
    n = n(),
    mean = mean(extra_rating),
    sd = sd(extra_rating),
    lower = mean - (1.96 * (sd / sqrt(n))),
    upper = mean + (1.96 * (sd / sqrt(n)))
  )

# Descriptives for simple-effects
zhang_long %>%
  group_by(condition, extra_time) %>%
  summarize(
    n = n(),
    mean = mean(extra_rating),
    sd = sd(extra_rating),
    lower = mean - (1.96 * (sd / sqrt(n))),
    upper = mean + (1.96 * (sd / sqrt(n)))
  )






# Split data by condition
ordinary <- zhang_long %>% filter(condition == "ordinary")
extraordinary <- zhang_long %>% filter(condition == "extraordinary")
time_1 <- zhang_long %>% filter(extra_time == "t1_extra")
time_2 <- zhang_long %>% filter(extra_time == "t2_extra")

############### SIMPLE-EFFECTS TEST OPTION 1

# Tukey HSD; no F-alue
library(agricolae)

# Split data along within-subjects variable
time_1 <- zhang_long %>% filter(extra_time == "t1_extra")
time_2 <- zhang_long %>% filter(extra_time == "t2_extra")

# Create analysis of variance
extra_t1_out <- aov(extra_rating ~ condition, data = time_1)
extra_t2_out <- aov(extra_rating ~ condition, data = time_2)

# Analyze with Tukey HSD
HSD.test(extra_t1_out, "condition", group = FALSE, console = TRUE)
HSD.test(extra_t2_out, "condition", group = FALSE, console = TRUE)





############### SIMPLE-EFFECTS TEST OPTION 2
extra_t1_out <- lme(extra_rating ~ condition, random = ~1|subject_id, data = time_1, method = "ML")
extra_t2_out <- lme(extra_rating ~ condition, random = ~1|subject_id, data = time_2, method = "ML")

options(contrasts = c("contr.sum", "contr.poly"))
Anova(lme(extra_rating ~ extra_time, random = ~1|subject_id, data = extraordinary, method = "ML"), type = "III")

output <- lme(extra_rating ~ extra_time, random = ~1|subject_id, data = ordinary, method = "ML")

summary(aov(extra_rating ~ extra_time + Error(subject_id), data = ordinary))



ezANOVA(data = ordinary,
  dv = extra_rating,
  wid = subject_id,
  within = extra_time,
  detailed = TRUE,
  type = 3
)



#### Outliers?

#### Normality?

#### Homoscedasticity?
leveneTest(extra_rating ~ condition, data = zhang_long, center = mean)

#### Sphericity?

####################################################################################

####### CURIOSITY OVER TIME BY CONDITION #######

# Create aov object
curious_aov <- aov(curious_rating ~ condition*curious_time + Error(subject_id),
                   data = zhang_long)

# Summary of model
summary(curious_aov)

# Tidy output
(curious_tidied <- tidy(curious_aov))

# Partial eta-squared for main effect of time
curious_tidied$sumsq[3] / (curious_tidied$sumsq[3] + curious_tidied$sumsq[5])

# Partial eta-squared for interaction of time and condition
curious_tidied$sumsq[4] / (curious_tidied$sumsq[4] + curious_tidied$sumsq[5])

# Create linear mixed-effects model
curious_lme <- lme(curious_rating ~ condition*curious_time, random = ~1|subject_id,
                   data = zhang_long)


# Print ANOVA summary using type III sum of squares
options(contrasts = c("contr.sum", "contr.poly"))
Anova(curious_lme, type = "III")

# Descriptives for time
zhang_long %>%
  group_by(curious_time) %>%
  summarize(
    n = n(),
    mean = mean(curious_rating),
    sd = sd(curious_rating),
    lower = mean - (1.96 * (sd / sqrt(n))),
    upper = mean + (1.96 * (sd / sqrt(n)))
  )

# Descriptives for simple-effects
zhang_long %>%
  group_by(condition, curious_time) %>%
  summarize(
    n = n(),
    mean = mean(curious_rating),
    sd = sd(curious_rating),
    lower = mean - (1.96 * (sd / sqrt(n))),
    upper = mean + (1.96 * (sd / sqrt(n)))
  )

#### Outliers?

#### Normality?

#### Homoscedasticity?

#### Sphericity?

####################################################################################

####### INTEREST OVER TIME BY CONDITION #######

# Create aov object
interest_aov <- aov(interest_rating ~ condition*interest_time + Error(subject_id),
                    data = zhang_long)

# Summarize the model
summary(interest_aov)

# Tidy output
(interest_tidied <- tidy(interest_aov))

# Partial eta-squared for main effect of time
interest_tidied$sumsq[3] / (interest_tidied$sumsq[3] + interest_tidied$sumsq[5])

# Partial eta-squared for interaction between time and condition
interest_tidied$sumsq[4] / (interest_tidied$sumsq[4] + interest_tidied$sumsq[5])

# Create linear mixed-effects model
interest_lme <- lme(interest_rating ~ condition*interest_time, random = ~1|subject_id,
                    data = zhang_long)

# Print ANOVA summary using type III sum of squares
options(contrasts = c("contr.sum", "contr.poly"))
Anova(interest_lme, type = "III")

# Descriptives for time
zhang_long %>%
  group_by(interest_time) %>%
  summarize(
    n = n(),
    mean = mean(interest_rating),
    sd = sd(interest_rating),
    lower = mean - (1.96 * (sd / sqrt(n))),
    upper = mean + (1.96 * (sd / sqrt(n)))
  )

# Descriptives for simple-effects
zhang_long %>%
  group_by(condition, interest_time) %>%
  summarize(
    n = n(),
    mean = mean(interest_rating),
    sd = sd(interest_rating),
    lower = mean - (1.96 * (sd / sqrt(n))),
    upper = mean + (1.96 * (sd / sqrt(n)))
  )

#### Outliers?

#### Normality?

#### Homoscedasticity?

#### Sphericity?

####################################################################################

####### VISUALIZE #######

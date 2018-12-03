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
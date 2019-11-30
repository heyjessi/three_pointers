# Jess, Anna and Seth Project
# 11/6/19
library(tidyverse)
library(dplyr)

teams34 <- read.csv('data/teams0304.csv')
teams45 <- read.csv('data/teams0405.csv')
teams56 <- read.csv('data/teams0506.csv')
teams67 <- read.csv('data/teams0607.csv')
teams78 <- read.csv('data/teams0708.csv')
teams89 <- read.csv('data/teams0809.csv')

names(teams34)
predictors <- c("")

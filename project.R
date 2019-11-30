# Jess, Anna and Seth Project
# 11/29/19
library(tidyverse)
library(dplyr)

read_data <- function() {
  teams0304 <- read.csv('data/teams0304.csv')
  teams0405 <- read.csv('data/teams0405.csv')
  teams0506 <- read.csv('data/teams0506.csv')
  teams0607 <- read.csv('data/teams0607.csv')
  teams0708 <- read.csv('data/teams0708.csv')
  teams0809 <- read.csv('data/teams0809.csv')
  teams0910 <- read.csv('data/teams0910.csv')
  teams1011 <- read.csv('data/teams1011.csv')
  teams1112 <- read.csv('data/teams1112.csv')
  teams1213 <- read.csv('data/teams1213.csv')
  teams1314 <- read.csv('data/teams1314.csv')
  teams1415 <- read.csv('data/teams1415.csv')
  teams1516 <- read.csv('data/teams1516.csv')
  teams1617 <- read.csv('data/teams1617.csv')
  teams1718 <- read.csv('data/teams1718.csv')
}

read_data()

names(teams0304)
predictors <- c("")

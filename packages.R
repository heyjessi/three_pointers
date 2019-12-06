# Jess, Anna and Seth Project Packages
# 12/6/19
if (!require('optimx')) install.packages('optimx'); library(optimx)
if (!require('parallel')) install.packages('parallel'); library(parallel)
if (!require('minqa')) install.packages('minqa'); library(minqa)
if (!require('lme4')) install.packages('lme4'); library(lme4) # for mixed models 
if (!require('segmented')) install.packages('segmented'); library(segmented)
# https://cran.r-project.org/web/packages/segmented/segmented.pdf
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
#if (!require('hrbrthemes')) install.packages('hrbrthemes'); library(hrbrthemes)
if (!require('ggcorrplot')) install.packages('ggcorrplot'); library(ggcorrplot)
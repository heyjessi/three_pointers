# Jess, Anna and Seth Project
# X3P.
# 11/29/19
source('styleguide.R')
source('helpers.R')
# Packages for optimizers
if (!require('optimx')) install.packages('optimx'); library(optimx)
if (!require('parallel')) install.packages('parallel'); library(parallel)
if (!require('minqa')) install.packages('minqa'); library(minqa)
if (!require('lme4')) install.packages('lme4'); library(lme4) # for mixed models 
if (!require('segmented')) install.packages('segmented'); library(segmented)
# https://cran.r-project.org/web/packages/segmented/segmented.pdf

# Read in Clean DF 
df.clean <- add_time("complete_data_clean.csv")
df.tourney <- add_time("tourney_data_clean.csv")
names(df.tourney)

# Check dimensions - len(unique schools) * len(unique years) must equal # of rows
dim_checker(df.clean)
dim_checker(df.tourney)


# since we know that games are increasing can we make those statistics into proportions to control
# for the specific effect
get_newprop = cbind(df.tourney$School, get_prop_df(df.tourney))
get_newprop
names(get_newprop)
# Let's have X3PAr be our response
# Check assumption of normal distribution
p <- ggplot(get_newprop, aes(x=X3P.)) +
  geom_histogram(colour="black", fill='#EE3838') + 
  labs(title="3P. Histogram") +
  xlab("3P.") +
  ylab("Counts") +
  theme_hodp()
p

#### EDA ####

p <- ggplot(get_newprop, aes(x = time + 2003, y = X3P)) + 
  geom_point() +
  stat_smooth(method = "lm", col = '#EE3838', se = F) +   
  labs(title="3P Made per Game over Time") +
  xlab("Year") +
  ylab("3P Made per Game") +
  #ylim(c(0, 0.6)) + 
  theme_hodp()
p

# 3PA
p <- ggplot(get_newprop, aes(x = time + 2003, y = X3PA)) + 
  geom_point() +
  stat_smooth(method = "lm", col = '#EE3838', se = F) +   
  labs(title="3P Attempted per Game over Time") +
  xlab("Year") +
  ylab("3P.") +
  #ylim(c(0,0.6)) + 
  theme_hodp()
p

# 3P percentage
p <- ggplot(get_newprop, aes(x = time + 2003, y = X3P.)) + 
  geom_point() +
  stat_smooth(method = "lm", col = '#EE3838', se = F) +   
  labs(title="3P Attempted per Game over Time") +
  xlab("Year") +
  ylab("3P.") +
  #ylim(c(0,0.6)) + 
  theme_hodp()
p



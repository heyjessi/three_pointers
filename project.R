# Jess, Anna and Seth Project
# 11/29/19
setwd("stat139finalproject/") # for jess only

source('styleguide.R')
source('helpers.R')
source('cleaner.R')
# Packages for optimizers
if (!require('optimx')) install.packages('optimx'); library(optimx)
if (!require('parallel')) install.packages('parallel'); library(parallel)
if (!require('minqa')) install.packages('minqa'); library(minqa)
if (!require('lme4')) install.packages('lme4'); library(lme4) # for mixed models 
if (!require('segmented')) install.packages('segmented'); library(segmented)
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
# https://cran.r-project.org/web/packages/segmented/segmented.pdf

# Read in Clean DF 
df.clean <- add_time("complete_data_clean.csv")
df.clean <- add_coach_change(df.clean)
df.tourney <- add_time("tourney_data_clean.csv")
df.tourney <- add_coach_change(df.tourney)

# Check dimensions - len(unique schools) * len(unique years) must equal # of rows
dim_checker(df.clean)
dim_checker(df.tourney)

# since we know that games are increasing can we make those statistics into proportions to control
# for the specific effect
get_newprop = cbind(df.tourney$School, get_prop_df(df.tourney))
get_newprop

# Let's have X3PAr be our response
# Check assumption of normal distribution
p <- ggplot(df.tourney, aes(x=X3PAr)) +
  geom_histogram(colour="black", fill='#EE3838') + 
  labs(title="3PAr Histogram") +
  xlab("3PAr") +
  ylab("Counts") +
  theme_hodp()
p

# Model 1: pool all teams together, OLS model for 3PAr change over time
lm1 <- lm(X3PAr ~ time, df.tourney)
summary(lm1)
names(df.clean)

p <- ggplot(df.tourney, aes(x = time + 2003, y = X3PAr)) + 
  geom_point() +
  stat_smooth(method = "lm", col = '#EE3838', se = F) +   
  labs(title="3PAr Over Time - Pooled") +
  xlab("Year") +
  ylab("3PAr") +
  #ylim(c(0,0.6)) + 
  theme_hodp()
p

# Model 2: Mixed Model, fixed effect of time, random intercept stratified on School
lmer2 <- lmer(X3PAr ~ time + (1 | School), data=df.tourney)
summary(lmer2)
coef(summary(lmer2))
coef(lmer2)$School

# split into categories -- > see if there are  
# binary indicator as for if they were a winning team or losing team for most of the seasons

### Fitting a random slopes, random intercepts model is often failing to converge
lmer3 <- lmer(X3PAr ~ time + (1  + time|School), data=df.tourney) # fails to converge

# list of convergence failures... 
lmer3a <- update(lmer3,
              REML = FALSE, 
              control = lmerControl(
                optimzer ='optimx', optCtrl=list(method='L-BFGS-B')))
lmer3b <- update(lmer3, 
                 control=lmerControl(optCtrl=list(ftol_abs=1e-8,xtol_abs=1e-8)))
lmer3c <- update(lmer3, control=lmerControl(optimizer="bobyqa"))

# Use all fit to find a model that converges
# Source: https://joshua-nugent.github.io/allFit/
ncores <- detectCores()
diff_optims <- allFit(lmer3, maxfun = 1e6, parallel = 'multicore', ncpus = ncores)
is.OK <- sapply(diff_optims, is, "merMod")
diff_optims.OK <- diff_optims[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)

### Nelder_Mead converges successfully!!! - but only for the df.tourney
lmer3d <- update(lmer3, control=lmerControl(optimizer="Nelder_Mead"))

# Summary
summary(lmer3d)

### COMPARE 
# Fixed coefs
coef(summary(lmer2))
coef(summary(lmer3d))

# Look at differences b/t individual schools coefs
head(coef(lmer2)$School)
head(coef(lmer3d)$School)

# Unsurprisingly, our random slopes and intercepts model is significantly better than 
# our simple random intercepts model. It may be even more overfit though. 
anova(lmer2,lmer3d)

### Let's do some plots
# Get coefficients 
year <- 2003:2017
intercept.mm <- summary(lmer3d)$coef[1,1]
slope.mm <- summary(lmer3d)$coef[2,1]
lmer3fn <- function(year) { 
  return(intercept.mm + (year - 2003) * slope.mm)
}

p <- ggplot(df.tourney, aes(x = time + 2003, y = X3PAr)) + 
  geom_point() +
  stat_smooth(method = "lm", col = '#EE3838', se = F) +  
  geom_segment(aes(x = 2003, y = lmer3fn(2003), xend = 2017, yend = lmer3fn(2017), colour = "yellow"), 
               data = df.tourney) + 
  labs(title="3PAr Over Time - Pooled") +
  xlab("Year") +
  ylab("3PAr") +
  #ylim(c(0,0.6)) + 
  theme_hodp()
p

# ... yeah it's just the same damn line which isn't surprising


### SEGMENTED REGRESSION ###
# Using the segmented package
# have to provide estimates for breakpoints.
# apriori guess of 10 based on Curry 2015 MVP season,   
seg4 <- segmented(lm1, 
                    seg.Z = ~ time, 
                    psi = list(time = c(10)))


summary(seg4)

# display the summary
summary(seg4)

# get breakpoints
seg4$psi
# get the slopes
slope(seg4)

# get the fitted data
my.fitted <- fitted(seg4)
my.model <- data.frame(year = df.tourney$year, X3PAr = my.fitted)

# plot the fitted model
ggplot(my.model, aes(x = year, y = X3PAr)) + geom_line()

# Replot things
cols <- c("Simple OLS"='#EE3838',"Segmented OLS"='#78C4D4')
p <- ggplot(df.tourney, aes(x = time + 2003, y = X3PAr)) + 
  geom_point() +
  stat_smooth(method = "lm", aes(col = '#EE3838'), se = F,size=1) + 
  geom_line(data = my.model, aes(x = year, y = X3PAr, color = '#78C4D4'),
            linetype = "solid", size=1) + 
  scale_colour_identity(name="Model Type", 
                        breaks = c('#EE3838','#78C4D4'),
                        labels = c("Simple OLS", "Segmented OLS break between 9 and 10"),
                        guide = "legend")  +   
  labs(title="3PAr Over Time - Pooled") +
  xlab("Year") +
  ylab("3PAr") +
  theme_hodp()
p 

# Let's try to find segements using psi = NA 
# This will iteratively try to find breakpoints though its likely to overestimate 
# the appropriate number 
seg5 <- segmented(lm1, 
                  seg.Z = ~ time, 
                  psi = NA)

summary(seg5)

# Suggests 2 breakpoints




### Alternative method to search: Test for Breakpoints
davies.test(lm1, ~time)

seg6 <- segmented(lm1, 
                  seg.Z = ~ time, 
                  psi = list(time = c(9.3)))
# Check for existence of one breakpoint using the pscore.test command

davies.test(seg4, ~time)

seg7 <- segmented(lm1, 
                  seg.Z = ~ time, 
                  psi = list(time = c(3.1, 9.3)))

davies.test(seg7, ~time)

# 2 Breakpoints
### between the 2006-2007 and 2007-08 seasons - Rule change was announced in May 2007 
# https://www.espn.com/mens-college-basketball/news/story?id=2859065

# Then another, more significant breakpoint between 2012-2013 and 2013-2014 seasons 
# Curry sets record for NBA 3's in 2012-13
# get breakpoints
seg7$psi
# get the slopes
slope(seg7)

# get the fitted data
seg7.fitted <- fitted(seg7)
seg7.fitted.df <- data.frame(year = df.tourney$year, X3PAr = seg7.fitted)

# plot the fitted model
ggplot(seg7.fitted.df, aes(x = year, y = X3PAr)) + geom_line()

p <- ggplot(df.tourney, aes(x = time + 2003, y = X3PAr)) + 
  geom_point() +
  stat_smooth(method = "lm", aes(col = '#EE3838'), se = F,size=1) + 
  geom_line(data = seg7.fitted.df, aes(x = year, y = X3PAr, color = '#78C4D4'),
            linetype = "solid", size=1) + 
  scale_colour_identity(name="Model Type", 
                        breaks = c('#EE3838','#78C4D4'),
                        labels = c("Simple OLS", "Segmented OLS: 2 breaks"),
                        guide = "legend")  +   
  labs(title="3PAr Over Time - Pooled") +
  xlab("Year") +
  ylab("3PAr") +
  theme_hodp()
p 

### T tests to determine whether or not slopes are significantly different (bonferroni needed?)
# https://influentialpoints.com/Training/simple_linear_regression-principles-properties-assumptions.htm
slope(seg7)

# Mixed Model Segmented - coaching change
lmer8 <- lmer(X3PAr ~ time*same.coach + (1  + time|School) , data=df.tourney) # fails to converge
# Use all fit to find a model that converges
# Source: https://joshua-nugent.github.io/allFit/
ncores <- detectCores()
diff_optims <- allFit(lmer8, maxfun = 1e6, parallel = 'multicore', ncpus = ncores)
is.OK <- sapply(diff_optims, is, "merMod")
diff_optims.OK <- diff_optims[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)

# Nelder_Mead for convergence
lmer8a <- update(lmer8, control=lmerControl(optimizer="Nelder_Mead"))

# Summary
summary(lmer8a)

### COMPARE 
# Fixed coefs
coef(summary(lmer8a))

# Look at differences b/w individual schools coefs
head(coef(lmer8a)$School)

### Let's do some plots
# Get coefficients 
year <- 2003:2017
intercept.false <- summary(lmer8a)$coef[1,1]
slope.false <- summary(lmer8a)$coef[2,1]
intercept.true <- summary(lmer8a)$coef[3,1]
slope.true <- summary(lmer8a)$coef[4,1]
lmer8fn <- function(year, true_flag) { 
  return(intercept.false + (year - 2003) * slope.false + 
           intercept.true*true_flag + slope.true * (year - 2003) * true_flag)
}

p <- ggplot(df.tourney, aes(x = time + 2003, y = X3PAr)) + 
  geom_point() +
  geom_segment(aes(x = 2003, y = lmer8fn(2003,0), xend = 2017, yend = lmer8fn(2017,0), 
                   colour = '#EE3838'), 
               data = df.tourney) + 
  geom_segment(aes(x = 2003, y = lmer8fn(2003,1), xend = 2017, yend = lmer8fn(2017,1), 
                   colour = '#78C4D4'), 
               data = df.tourney) +
  scale_colour_identity(name="Model Type", 
                        breaks = c('#EE3838','#78C4D4'),
                        labels = c("Coaching Change", "Same Coach"),
                        guide = "legend") +
  labs(title="3PAr Over Time - Pooled") +
  xlab("Year") +
  ylab("3PAr") +
  theme_hodp()
p


# Mixed model by era 
df.tourney$era <- as.factor((df.tourney$year > 2006) + (df.tourney$year > 2012))
# Mixed Model Segmented - coaching change
lmer9 <- lmer(X3PAr ~ time*era + (1  + time|School), data=df.tourney) # fails to converge
# Use all fit to find a model that converges
# Source: https://joshua-nugent.github.io/allFit/
ncores <- detectCores()
diff_optims <- allFit(lmer9, maxfun = 1e6, parallel = 'multicore', ncpus = ncores)
is.OK <- sapply(diff_optims, is, "merMod")
diff_optims.OK <- diff_optims[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)

### T tests to determine whether or not slopes are significantly different
# https://influentialpoints.com/Training/simple_linear_regression-principles-properties-assumptions.html

# Nelder_Mead for convergence
lmer9a <- update(lmer9, control=lmerControl(optimizer="Nelder_Mead"))
summary(lmer9a)

# draws the mean number of threes point attempts per year across years

### Compare

### COMPARE 
# Fixed coefs
coef(summary(lmer9a))

# Look at differences b/w individual schools coefs
head(coef(lmer9a)$School)

### Let's do some plots
# Get coefficients 
year <- 2003:2017
intercept.0 <- summary(lmer9a)$coef[1,1]
slope.0 <- summary(lmer9a)$coef[2,1]
intercept.1 <- summary(lmer9a)$coef[3,1]
intercept.2 <- summary(lmer9a)$coef[4,1]
slope.1 <- summary(lmer9a)$coef[5,1]
slope.2 <- summary(lmer9a)$coef[6,1]
lmer9fn <- function(year, era1, era2) { 
  return(intercept.0 + (year - 2003) * slope.0 + 
           intercept.1*era1 + slope.1 * (year - 2003) * era1 + 
           intercept.2*era2 + slope.2 * (year - 2003) * era2)
}
df.tourney


p <- ggplot(df.tourney, aes(x = time + 2003, y = X3PAr)) + 
  geom_point() +
  geom_segment(aes(x = 2003, y = lmer9fn(2003,0,0), xend = 2006, yend = lmer9fn(2006,0,0), 
                   colour = '#EE3838'), 
               data = df.tourney) + 
  geom_segment(aes(x = 2007, y = lmer9fn(2007,1,0), xend = 2012, yend = lmer9fn(2012,1,0), 
                   colour = '#78C4D4'), 
               data = df.tourney) +
  geom_segment(aes(x = 2013, y = lmer9fn(2013,0,1), xend = 2017, yend = lmer9fn(2017,0,1), 
                   colour = '#4B5973'), 
               data = df.tourney) +
  geom_vline(xintercept = 2012.5) +
  geom_vline(xintercept = 2006.5) +
  scale_colour_identity(name="Model Type:", 
                        breaks = c('#EE3838','#78C4D4','#4B5973'),
                        labels = c("2003-2006", "2007-2012", "2012-2013"),
                        guide = "legend") +
  annotate(geom="label", x = 2012.5, y = 0, label = "NBA Revolution", fill ="#F2F2F2", color = "black") +
  annotate(geom="label", x = 2006.5, y = 0, label = "NCAA Rule Change", fill ="#F2F2F2", color = "black") +
  labs(title="3PAr Over Time - Segmented Mixed Model") +
  xlab("Year") +
  ylab("3PAr") +
  theme_hodp()
p

# jess stuffs
df.tourney %>%
  group_by(time) %>%
  summarise(mean_three = mean(X3PAr)) %>%
  ggplot(df.tourney, mapping = aes(x = time + 2003, y = mean_three)) +
  geom_line(stat="identity") + ggtitle("Average 3PAr Across the League") + 
  xlab("Year") +
  ylab("3PAr")+
  theme_hodp()

# jess tests

# gets the minimum slopes, what patterns does this reveal

sorted_alpha_school = df.tourney[order(df.tourney$School),]
sorted_alpha_school$winner = (sorted_alpha_school$W.L. >= 0.5) * 1
View(sorted_alpha_school)

total_wins <- aggregate(sorted_alpha_school$winner, by=list(sorted_alpha_school$School), FUN=sum)[2]

idx_min <- which.min(coef(lmer3d)$School[,2])
school_list <- sort(unique(df.tourney$School)) # alphabetically sort the list
school_list[idx_min]
coef(lmer3d)$School[idx_min, 2]

new_df <- data.frame(school = school_list, slope = coef(lmer3d)$School[,2], wins = total_wins)
new_df = new_df[order(new_df$slope),]
nrow(new_df)
most_neg <- new_df[1:75, ]
middle <- new_df[75:150, ]
most_pos <- new_df[151:225, ]

ggplot(most_neg, aes(x = x)) +
  geom_histogram() +
  geom_vline(aes(xintercept=mean(total_wins$x)),
             color="red", size=1) +
  geom_vline(aes(xintercept=mean(most_neg$x)),
             color="blue",linetype="dashed", size=1) +
  theme_hodp() +
  labs(title="Bottom Third Teams For 3PAr Rate") +
  xlab("Wins in 15 years") +
  ylab("Frequency")

ggplot(most_pos, aes(x = x)) +
  geom_histogram() +
  geom_vline(aes(xintercept=mean(total_wins$x)),
             color="red", size=1) +
  geom_vline(aes(xintercept=mean(most_pos$x)),
             color="blue",linetype="dashed", size=1) +
  theme_hodp() +
  labs(title="Top Third Teams For 3PAr Rate") +
  xlab("Wins in 15 years") +
  ylab("Frequency")

# check for with absolute value
abs_df <- new_df
abs_df$slope = abs(abs_df$slope)
abs_df = abs_df[order(abs_df$slope),]
abs_df

close_zero <- abs_df[1:75, ]
close_zero

ggplot(close_zero, aes(x = x)) +
  geom_histogram() +
  geom_vline(aes(xintercept=mean(total_wins$x)),
             color="red", size=1) +
  geom_vline(aes(xintercept=mean(close_zero$x)),
             color="blue",linetype="dashed", size=1) +
  theme_hodp() +
  labs(title="Top Third of Teams Closest to 0 3PAr Rate Change") +
  xlab("Wins in 15 years") +
  ylab("Frequency")

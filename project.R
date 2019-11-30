# Jess, Anna and Seth Project
# 11/29/19
source('styleguide.R')
source('helpers.R')
# Packages for optimizers
if (!require('optimx')) install.packages('optimx'); library(optimx)
if (!require('parallel')) install.packages('parallel'); library(parallel)
if (!require('minqa')) install.packages('minqa'); library(minqa)
if (!require('lme4')) install.packages('lme4'); library(lme4) # for mixed models 

# Read in Clean DF 

df.clean <- add_time("complete_data_clean.csv")
df.tourney <- add_time("tourney_data_clean.csv")

# Check dimensions - len(unique schools) * len(unique years) must equal # of rows
dim_checker(df.clean)
dim_checker(df.tourney)


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

p <- ggplot(df.tourney, aes(x = time + 2003, y = X3P)) + 
  geom_point() +
  stat_smooth(method = "lm", col = '#EE3838', se = F) +   
  labs(title="3PAr Over Time - Pooled") +
  xlab("Year") +
  ylab("3PAr") +
  #ylim(c(0,0.6)) + 
  theme_hodp()
p

# Model 2: Mixed Model, fixed effect of time
lmer2 <- lmer(X3PAr ~ time + (1 | School), data=df.tourney)
summary(lmer2)
coef(summary(lmer2))
coef(lmer2)$School

### Fitting a random slopes, random intercepts model is often failing to converge
lmer3 <- lmer(X3PAr ~ time + (1|School) + (time|School), data=df.tourney)

# list of convergence failures... 
lmer3a <- update(lmer3,
              REML = FALSE, 
              control = lmerControl(
                optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
lmer3b <- update(lmer3, 
                 control=lmerControl(optCtrl=list(ftol_abs=1e-8,xtol_abs=1e-8)))
lmer3c <- update(lmer3, control=lmerControl(optimizer="bobyqa"))

# Use all fit to find a  
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

# Look at differences b/w individual schools coefs
coef(lmer2)$School
coef(lmer3d)$School


# Unsurprisingly, our random slopes and intercepts model is significantly better than 
# our simple random intercepts model. It may be even more overfit though. 
anova(lmer2,lmer3)

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

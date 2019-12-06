# Jess, Anna and Seth Project EDA
# 11/29/19
source('styleguide.R')
source('helpers.R')
source('packages.R')
source('cleaner.R')
# https://cran.r-project.org/web/packages/segmented/segmented.pdf

# Read in Clean DF 
df.clean <- add_time("complete_data_clean.csv")
df.tourney <- add_time("tourney_data_clean.csv")
names(df.tourney)

# Check dimensions - len(unique schools) * len(unique years) must equal # of rows
dim_checker(df.clean)
dim_checker(df.tourney)

# Per game-ify
get_newprop = cbind(df.tourney$School, get_prop_df(df.tourney))

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

#### histograms ####

# Let's have X3PAr be our response
# Check assumption of normal distribution
p <- ggplot(df.tourney, aes(x=X3PAr)) +
  geom_histogram(colour="black", fill='#EE3838') + 
  labs(title="3PAr Histogram") +
  xlab("3PAr") +
  ylab("Counts") +
  theme_hodp()
p

# X3P hist
p <- ggplot(get_newprop, aes(x = time + 2003, y = X3P)) + 
  geom_point() +
  stat_smooth(method = "lm", col = '#EE3838', se = F) +   
  labs(title="3P Made per Game over Time") +
  xlab("Year") +
  ylab("3P Made per Game") +
  #ylim(c(0, 0.6)) + 
  theme_hodp()
p

# 3PA Hist
p <- ggplot(get_newprop, aes(x = time + 2003, y = X3PA)) + 
  geom_point() +
  stat_smooth(method = "lm", col = '#EE3838', se = F) +   
  labs(title="3P Attempted per Game over Time") +
  xlab("Year") +
  ylab("3PA per Game") +
  #ylim(c(0,0.6)) + 
  theme_hodp()
p

# 3P percentage Hist
p <- ggplot(get_newprop, aes(x = time + 2003, y = X3P.)) + 
  geom_point() +
  stat_smooth(method = "lm", col = '#EE3838', se = F) +   
  labs(title="3P Percentage over Time") +
  xlab("Year") +
  ylab("3P.") +
  #ylim(c(0,0.6)) + 
  theme_hodp()
p

### AIC TABLE ### 
# Make AIC Table
simple_OLS_AIC = AIC(lm1)
lmer2_AIC = AIC(lmer2)
lmer3d_AIC = AIC(lmer3d)
lin_seg_AIC = AIC(seg7)
mm_final_AIC = AIC(lmer9a)

AICs = c(simple_OLS_AIC,lmer2_AIC, lmer3d_AIC, lin_seg_AIC,mm_final_AIC)
titles = c("Simple OLS",
           "Simple Random Intercepts", 
           "Simple Random Slopes and Intercepts", 
           "Segmented OLS", 
           "Segmented Random Slopes and Intercepts")
data.frame(titles, AICs)



#### Games Increasing ####

# since we know that games are increasing can we make those statistics into 
# proportions to control for the specific effect
get_newprop = cbind(df.tourney$School, get_prop_df(df.tourney))
get_newprop

df.clean.noschool = df.clean[,2:length(df.clean)]
top_cor_list = cor(df.clean.noschool)[,ncol(df.clean.noschool)-1]
top_cor_list = sort(top_cor_list, decreasing = TRUE)
top_cor_list = top_cor_list[3:length(top_cor_list)]
top_cor_list
list_top = names(top_cor_list)
list_top

# graphs
df.clean.noschool %>%
  group_by(time) %>%
  summarise(mean_games = mean(G)) %>%
  ggplot(df.clean.noschool, mapping = aes(x = time + 2003, y = mean_games)) +
  geom_line(stat="identity") + ggtitle("Games Played Per Season in the NCAA") + 
  ylim(25, 35) +
  xlab("Year") +
  ylab("Games")+
  theme_hodp()

# we noticed that games also increases over time (it's one of the top predictors)
plot(df.clean.noschool$time, df.clean.noschool$G)

### MEANS PLOTS ###

# EDA plot to show how average 3Ar changes with time
df.tourney %>%
  group_by(time) %>%
  summarise(mean_three = mean(X3PAr)) %>%
  ggplot(df.tourney, mapping = aes(x = time + 2003, y = mean_three)) +
  geom_line(stat="identity") + ggtitle("Average 3PAr Across the NCAA") + 
  xlab("Year") +
  ylab("3PAr")+
  theme_hodp()


# Jess, Anna and Seth Project
# 12/6/19
setwd("stat139finalproject/") # for jess only

source('packages.R')
source('styleguide.R')
source('helpers.R')
source('cleaner.R')

# Read in Clean DF 
df.clean <- add_time("complete_data_clean.csv")
df.clean <- add_coach_change(df.clean)
df.tourney <- add_time("tourney_data_clean.csv")
df.tourney <- add_coach_change(df.tourney)

# Check dimensions - len(unique schools) * len(unique years) must equal # of rows
dim_checker(df.clean)
dim_checker(df.tourney)

# gets the minimum slopes, what patterns does this reveal
sorted_alpha_school = df.tourney[order(df.tourney$School),]
sorted_alpha_prop = get_newprop[order(get_newprop$`df.tourney$School`),]
sorted_alpha_school$winner = (sorted_alpha_school$W.L. >= 0.5) * 1
View(sorted_alpha_school)

total_wins <- aggregate(sorted_alpha_school$winner, by=list(sorted_alpha_school$School), FUN=sum)[2]
total_3s_made <- aggregate(sorted_alpha_prop$X3P, by=list(sorted_alpha_prop$`df.tourney$School`), FUN=mean)[2]
total_3s_attempted <- aggregate(sorted_alpha_prop$X3PA, by=list(sorted_alpha_prop$`df.tourney$School`), FUN=mean)[2]
total_sos <- aggregate(sorted_alpha_school$SOS, by=list(sorted_alpha_school$School), FUN=sum)[2]

idx_min <- which.min(coef(lmer3d)$School[,2])
school_list <- sort(unique(df.tourney$School)) # alphabetically sort the list
school_list[idx_min]
coef(lmer3d)$School[idx_min, 2]

new_df <- data.frame(school = school_list, slope = coef(lmer3d)$School[,2], wins = total_wins, threes_m = total_3s_made, sos = total_sos)
new_df = new_df[order(new_df$slope),]

# check for with absolute value
abs_df <- new_df
abs_df$slope = abs(abs_df$slope)
abs_df = abs_df[order(abs_df$slope),]
abs_df

close_zero_least <- abs_df[1:50,]
close_zero
close_zero_most <- abs_df[181:230,]

# histograms - teams least prone

ggplot(close_zero_least, aes(x = x)) +
  geom_histogram() +
  geom_vline(aes(xintercept=mean(total_wins$x)),
             color="red", size=1) +
  geom_vline(aes(xintercept=mean(close_zero_least$x)),
             color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(close_zero_most$x)),
             color="green",linetype="dashed", size=1) +
  theme_hodp() +
  labs(title="Teams Least Prone to 3PAr Rate Change") +
  xlab("Wins in 15 years") +
  ylab("Frequency")

ggplot(close_zero_least, aes(x = x.1)) +
  geom_histogram() +
  geom_vline(aes(xintercept=mean(total_3s_made$x)),
             color="red", size=1) +
  geom_vline(aes(xintercept=mean(close_zero_least$x.1)),
             color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(close_zero_most$x.1)),
             color="green",linetype="dashed", size=1) +
  theme_hodp() +
  labs(title="Teams Least Prone to 3PAr Rate Change") +
  theme(legend.position = c(0.8, 0.2)) +
  xlab("3 Pointers Made") +
  ylab("Frequency")

# histogram teams most prone

ggplot(close_zero_most, aes(x = x)) +
  geom_histogram() +
  geom_vline(aes(xintercept=mean(total_wins$x)),
             color="red", size=1) +
  geom_vline(aes(xintercept=mean(close_zero_most$x)),
             color="green",linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(close_zero_least$x)),
             color="blue",linetype="dashed", size=1) +
  theme_hodp() +
  labs(title="Teams Most Prone to 3PAr Rate Change") +
  xlab("Wins in 15 years") +
  ylab("Frequency")

ggplot(close_zero_most, aes(x = x.1)) +
  geom_histogram() +
  geom_vline(aes(xintercept=mean(total_3s_made$x)),
             color="red", size=1) +
  geom_vline(aes(xintercept=mean(close_zero_most$x.1)),
             color="green",linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(close_zero_least$x.1)),
             color="blue",linetype="dashed", size=1) +
  theme_hodp() +
  labs(title="Teams Most Prone to 3PAr Rate Change") +
  theme(legend.position = c(0.8, 0.2)) +
  xlab("3 Pointers Made") +
  ylab("Frequency")

# between the most and least affected teams - games
t.test(close_zero_least$x, close_zero_most$x)

# between the msot and least affected - 3 pointers made per game
t.test(close_zero_least$x.1, close_zero_most$x.1)

# more tests on the piece slopes
piece.slopes = coef(lmer9a)$School
piece.slopes$time = abs(piece.slopes$time)
piece.slopes = piece.slopes[order(piece.slopes$time),]

era.zeroslopes = piece.slopes[, 2]
era.oneslopes = era.zeroslopes + piece.slopes[, 5]
era.twoslopes = era.zeroslopes + piece.slopes[, 6]

piece_df <- data.frame(school = school_list, slope0 = era.zeroslopes, slope1 = era.oneslopes, slope2 = era.twoslopes, wins = total_wins, threes_m = total_3s_made)
piece_df0 = piece_df[order(piece_df$slope0),]
piece_df1 = piece_df[order(piece_df$slope1),]
piece_df2 = piece_df[order(piece_df$slope2),]

piece_df0_least <- piece_df0[1:50, ]
piece_df0_most <- piece_df0[(nrow(piece_df0) - 50):nrow(piece_df0), ]
piece_df1_least <- piece_df1[1:50, ]
piece_df1_most <- piece_df1[(nrow(piece_df1) - 50):nrow(piece_df1), ]
piece_df2_least <- piece_df2[1:50, ]
piece_df2_most <- piece_df2[(nrow(piece_df2) - 50):nrow(piece_df2), ]

# t tests for wins
mean(total_wins$x)
t.test(piece_df0_least$x, piece_df0_most$x)
t.test(piece_df1_least$x, piece_df1_most$x)
t.test(piece_df2_least$x, piece_df2_most$x)

# t tests for 3 pointers made, p value 0.05
mean(total_3s_made$x)
t.test(piece_df0_least$x.1, piece_df0_most$x.1)
t.test(piece_df1_least$x.1, piece_df1_most$x.1)
t.test(piece_df2_least$x.1, piece_df2_most$x.1)

mean(most_neg$x.2)
mean(most_pos$x.2)
mean(close_zero$x.2)
mean(total_sos$x)

#annas stuff

#eda correlation
data <- read.csv('data/full_data_raw.csv')
wl <- data %>% select(TeamW, TeamL, W.L., ConfW, ConfL, HomeW, HomeL, AwayW, AwayL)
cor <- round(cor(wl), 1)
p <- ggcorrplot(cor) + 
  labs(title='Corr Plot for W-L Vars') +
  xlab('') + ylab('') + 
  theme_hodp() + 
  theme(axis.text.x=element_text(angle=60))
p


#look at effects for individual teams
uva = df.tourney[df.tourney$School == 'Virginia',]
nova = df.tourney[df.tourney$School == 'Villanova',]
data_teams = rbind(uva, nova)

#plot villanova and overall linear fit
p <- ggplot(nova, aes(x = time + 2003, y = X3PAr)) + 
  geom_point() +
  stat_smooth(method = "lm", col = '#EE3838', se = F) +   
  labs(title="3PAr Over Time - Pooled") +
  xlab("Year") +
  ylab("3PAr") +
  #ylim(c(0,0.6)) + 
  theme_hodp()
p

#plot villanova and uva by color
p <- ggplot(data_teams, aes(x = time + 2003, y = X3PAr, color = School)) + 
  geom_point(aes(color = School))
p

#interaction between time and school
lmer9 <- lmer(X3PAr ~ time*era + (1  + time|School), data=data_teams) # fails to converge
lm2 <- lm(X3PAr ~ time*School, data_teams)
summary(lm2)
names(df.clean)


#interaction between school and time and time and era
data_teams$era <- as.factor((data_teams$year > 2006) + (data_teams$year > 2012))
lm3 <- lm(X3PAr ~ time*(School + era), data_teams)
lmer10 <- lmer(X3PAr ~ time*era + (1  + time|School), data=data_teams) # fails to converge

# Compare mixed model to lm 
summary(lm3)
summary(lmer10)
AIC(lm3)
AIC(lmer10)
# lm outperforms mixed model

intercept.0 <- summary(lm3)$coef[1,1]t
slope.0 <- summary(lm3)$coef[2,1]
intercept.virgina <- summary(lm3)$coef[3,1]
intercept.1 <- summary(lm3)$coef[4,1]
intercept.2 <- summary(lm3)$coef[5,1]
slope.virginia <- summary(lm3)$coef[6,1]
slope.1 <- summary(lm3)$coef[7,1]
slope.2 <- summary(lm3)$coef[8,1]

#plot lm3
year <- 2003:2017

fnnova <-  function(year, era1, era2) { 
  return(intercept.0 + (year - 2003) * slope.0 + 
           intercept.1*era1 + slope.1 * (year - 2003) * era1 + 
           intercept.2*era2 + slope.2 * (year - 2003) * era2)
}

fnuva <- function(year, era1, era2) { 
  return(intercept.0 + intercept.virgina + (year - 2003 + slope.virgina) * slope.0 + 
           intercept.1*era1 + slope.1 * (year - 2003) * era1 + 
           intercept.2*era2 + slope.2 * (year - 2003) * era2)
}

#plot interaction between school and time and time and era for just uva
p <- ggplot(data_teams, aes(x = time + 2003, y = X3PAr)) + 
  geom_point() +
  geom_segment(aes(x = 2003, y = fnuva(2003,0,0), xend = 2006, yend = fnuva(2006,0,0), 
                   colour = '#EE3838'), 
               data = data_teams) + 
  geom_segment(aes(x = 2007, y = fnuva(2007,1,0), xend = 2012, yend = fnuva(2012,1,0), 
                   colour = '#78C4D4'), 
               data = data_teams) +
  geom_segment(aes(x = 2013, y = fnuva(2013,0,1), xend = 2017, yend = fnuva(2017,0,1), 
                   colour = '#4B5973'), 
               data = data_teams) +
  geom_vline(xintercept = 2012.5) +
  geom_vline(xintercept = 2006.5) +
  scale_colour_identity(name="Model Type:", 
                        breaks = c('#EE3838','#78C4D4','#4B5973'),
                        labels = c("2003-2006", "2007-2012", "2012-2013"),
                        guide = "legend") +
  annotate(geom="label", x = 2012.5, y = 0, label = "NBA Revolution", fill ="#F2F2F2", color = "black") +
  annotate(geom="label", x = 2006.5, y = 0, label = "NCAA Rule Change", fill ="#F2F2F2", color = "black") +
  labs(title="3PAr Over Time - Villanova") +
  xlab("Year") +
  ylab("3PAr") +
  theme_hodp()
p


#plot interaction between school and time and time and era for both schools
p <- ggplot(data_teams, aes(x = time + 2003, y = X3PAr, color = School)) + 
  geom_point(aes(color = School)) +
  scale_color_manual(values =  c("#0000FF", "#00FFFF", "#0000FF", "#00FFFF"))+
  #geom_point(color = c('#EE3838', '#78C4D4')) +
  geom_segment(aes(x = 2003, y = fnnova(2003,0,0), xend = 2006, yend = fnnova(2006,0,0), 
                   colour = 'Villanova'), 
               data = data_teams) + 
  geom_segment(aes(x = 2007, y = fnnova(2007,1,0), xend = 2012, yend = fnnova(2012,1,0), 
                   colour = 'Villanova'), 
               data = data_teams) +
  geom_segment(aes(x = 2013, y = fnnova(2013,0,1), xend = 2017, yend = fnnova(2017,0,1), 
                   colour = 'Villanova'), 
               data = data_teams) +
  
  geom_segment(aes(x = 2003, y = fnuva(2003,0,0), xend = 2006, yend = fnuva(2006,0,0), 
                   colour = 'Virginia'), 
               data = data_teams) + 
  geom_segment(aes(x = 2007, y = fnuva(2007,1,0), xend = 2012, yend = fnuva(2012,1,0), 
                   colour = 'Virginia'), 
               data = data_teams) +
  geom_segment(aes(x = 2013, y = fnuva(2013,0,1), xend = 2017, yend = fnuva(2017,0,1), 
                   colour = 'Virginia'), 
               data = data_teams) +
  geom_vline(xintercept = 2012.5) +
  geom_vline(xintercept = 2006.5) +
  annotate(geom="label", x = 2012.5, y = 0, label = "NBA Revolution", fill ="#F2F2F2", color = "black") +
  annotate(geom="label", x = 2006.5, y = 0, label = "NCAA Rule Change", fill ="#F2F2F2", color = "black") +
  labs(title="3PAr Over Time By School") +
  xlab("Year") +
  ylab("3PAr") +
  theme_hodp()
p



#interaction between school and time and time and era and school and era
data_teams$era <- as.factor((data_teams$year > 2006) + (data_teams$year > 2012))
lm4 <- lm(X3PAr ~ time*(School + era) +School*era , data_teams)
summary(lm4)

intercept.0 <- summary(lm4)$coef[1,1]
slope.0 <- summary(lm4)$coef[2,1]
intercept.virgina <- summary(lm4)$coef[3,1]
intercept.1 <- summary(lm4)$coef[4,1]
intercept.2 <- summary(lm4)$coef[5,1]
slope.virginia <- summary(lm4)$coef[6,1]
slope.1 <- summary(lm4)$coef[7,1]
slope.2 <- summary(lm4)$coef[8,1]
intercept.1.virginia <- summary(lm4)$coef[9,1]
intercept.2.virginia <- summary(lm4)$coef[10,1]


year <- 2003:2017
fnnova <-  function(year, era1, era2) { 
  return(intercept.0 + (year - 2003) * slope.0 + 
           intercept.1*era1 + slope.1 * (year - 2003) * era1 + 
           intercept.2*era2 + slope.2 * (year - 2003) * era2)
}
fnuva <- function(year, era1, era2) { 
  return(intercept.0 + intercept.virgina + (year - 2003 + slope.virgina) * slope.0 + 
           (intercept.1 + intercept.1.virginia) *era1 + slope.1 * (year - 2003) * era1 + 
           (intercept.2 + intercept.2.virginia) *era2 + slope.2 * (year - 2003) * era2)
}

p <- ggplot(data_teams, aes(x = time + 2003, y = X3PAr, color = School)) + 
  geom_point(aes(color = School)) +
  scale_color_manual(values =  c("#EE3838", "#0000FF", "#EE3838", "#0000FF"))+
  #geom_point(color = c('#EE3838', '#78C4D4')) +
  geom_segment(aes(x = 2003, y = fnnova(2003,0,0), xend = 2006, yend = fnnova(2006,0,0), 
                   colour = 'Villanova'), 
               data = data_teams) + 
  geom_segment(aes(x = 2007, y = fnnova(2007,1,0), xend = 2012, yend = fnnova(2012,1,0), 
                   colour = 'Villanova'), 
               data = data_teams) +
  geom_segment(aes(x = 2013, y = fnnova(2013,0,1), xend = 2017, yend = fnnova(2017,0,1), 
                   colour = 'Villanova'), 
               data = data_teams) +
  
  geom_segment(aes(x = 2003, y = fnuva(2003,0,0), xend = 2006, yend = fnuva(2006,0,0), 
                   colour = 'Virginia'), 
               data = data_teams) + 
  geom_segment(aes(x = 2007, y = fnuva(2007,1,0), xend = 2012, yend = fnuva(2012,1,0), 
                   colour = 'Virginia'), 
               data = data_teams) +
  geom_segment(aes(x = 2013, y = fnuva(2013,0,1), xend = 2017, yend = fnuva(2017,0,1), 
                   colour = 'Virginia'), 
               data = data_teams) +
  geom_vline(xintercept = 2012.5) +
  geom_vline(xintercept = 2006.5) +
  annotate(geom="label", x = 2012.5, y = 0, label = "NBA Revolution", fill ="#F2F2F2", color = "black") +
  annotate(geom="label", x = 2006.5, y = 0, label = "NCAA Rule Change", fill ="#F2F2F2", color = "black") +
  labs(title="3PAr Over Time By School") +
  xlab("Year") +
  ylab("3PAr") +
  theme_hodp()
p



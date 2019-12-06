# Jess, Anna and Seth Project
# 11/29/19
source('helpers.R')
source('packages.R')

### READ IN THE DATA ###
# Create full dataframe
long.data <- read_data()

# Write long dataframe and read it in 
write.csv(long.data, file = "data/full_data_raw.csv")
full_df_raw <- read.csv("data/full_data_raw.csv")

# Let's remove certain predictors that we know from data collection are bad...
full_df <- select (full_df_raw,-c(Pace, ORtg, STL., ORB., MP, X, Rk, 
                                  # ... and some highly Colinear predictors 
                                  ConfW, ConfL, HomeW, HomeL, AwayW, AwayL))





### FIX SCHOOL NAMES ###
# We've got a problem with the School names
table(full_df$School)

# The source of our data adds " NCAA" to teams that made a tournament appearance 
# We need to combine these names with the school name if they didn't make an appearance

# Let's use some regex magic
schools <- as.character(full_df$School)
unique_schools <- names(table(full_df$School)); head(unique_schools)
schools_clean <- gsub("[[:space:]]+NCAA", "", schools)
unique_schools_clean <- names(table(schools_clean))

# Replace Schools with Schools clean 
full_df$School <- as.factor(schools_clean)

# look again at a table of schools - we're good
table(full_df$School)



### MISSING VALUES ###
# Check that values make sense in the summary 
summary(full_df)

# Fortunately, we don't have any missing values for anything relating to 3-pointers 
# There are a few missing values, but I'm not certain we'll even use those columns 
# as predictors, so we'll cross that bridge when we get there. 


### SUBSET DF INTO COMPLETE AND INCOMPLETE ###
# We still have some schools that don't have data for all of the years we want to study
# Let's remove all of the schools with incomplete data and put them in a separate data frame 
# for now. I'm not really sure what to do with them.
idx_complete <- which(table(full_df$School) == length(unique(full_df$year)))
schools_incomplete <- names(table(schools_clean))[-idx_complete]
df_incomplete <- full_df[which((full_df$School %in% schools_incomplete)), ]
write.csv(df_incomplete, file = "data/incomplete_data_clean.csv")


# Now let's subset our df for rows with complete data
df_complete <- full_df[which(!(full_df$School %in% schools_incomplete)), ]

# We have 322 unique schools with complete data from the 2003-2017 seasons
length(unique(df_complete$School))


# Write Clean Df
write.csv(df_complete, file = "data/complete_data_clean.csv")


### Tournament Teams for Simpler Model ### 

# I'm having issues with convergence. Might be best to restrict number of teams 
full_df_raw <- read.csv("data/full_data_raw.csv")
full_df <- select (full_df_raw,-c(Pace, ORtg, STL., ORB., MP, X, Rk, 
                                  ConfW, ConfL, HomeW, HomeL, AwayW, AwayL))

# Let's use some regex magic
schools <- as.character(full_df$School)
unique_schools <- names(table(full_df$School)); head(unique_schools)
schools_clean <- gsub("[[:space:]]+NCAA", "", schools)
unique_schools_clean <- names(table(schools_clean))

# Replace Schools with Schools clean 
full_df$School <- as.factor(schools_clean)

# Get just tourney schools
tourney_teams_idx <- which(grepl("NCAA", unique_schools))
tourney_teams <- unique_schools[tourney_teams_idx]
tourney_teams_clean <- gsub("[[:space:]]+NCAA", "", tourney_teams)
df_tourney <- full_df[which((full_df$School %in% tourney_teams_clean)), ]
table(df_tourney$School) 

# Check for completeness
idx_complete <- which(table(df_tourney$School) == length(unique(df_tourney$year)))
schools_incomplete <- names(table(schools_clean))[-idx_complete]
df_tourney_complete <- df_tourney[which(!(df_tourney$School %in% schools_incomplete)), ]

table(df_tourney_complete$School)

# Write csv
write.csv(df_tourney_complete, file = "data/tourney_data_clean.csv")



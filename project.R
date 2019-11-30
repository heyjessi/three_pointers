# Jess, Anna and Seth Project
# 11/29/19
library(tidyverse)
library(dplyr)

df_names <- c("teams0304.csv", "teams0405.csv", "teams0506.csv",
              "teams0708.csv", "teams0809.csv",
              "teams0910.csv", "teams1011.csv", "teams1112.csv", 
              "teams1213.csv", "teams1314.csv", "teams1415.csv", 
              "teams1516.csv", "teams1617.csv", "teams1718.csv")

read_data <- function() {
  df_names <- c("teams0405.csv", "teams0506.csv",
              "teams0708.csv", "teams0809.csv",
              "teams0910.csv", "teams1011.csv", "teams1112.csv", 
              "teams1213.csv", "teams1314.csv", "teams1415.csv", 
              "teams1516.csv", "teams1617.csv", "teams1718.csv")
  
  df <- read.csv('data/teams0304.csv')
  df$year <- 2003

  for (name in df_names) { 
    teams <- read.csv(paste('data/', name, sep=""))
    year <- paste("20", substr(name, 6, 7), sep = "")
    teams$year <- as.numeric(year)
    df <- rbind(df, teams)
  }
  
  return(df)
}

# Create full dataframe
long.data <- read_data()

# Write long dataframe and read it in 
# write.csv(long.data, file = "data/full_data.csv")
full_df <- read.csv("data/full_data.csv")

# Let's remove certain predictors that we know from data collection are bad
full_df <- select (full_df,-c(Pace, ORtg, STL., ORB., MP))

# We've got a problem with the School names
table(full_df$School)

# Need to unique and combine these 
schools <- as.character(full_df$School)
unique_schools <- names(table(full_df$School)); head(unique_schools)

# Regex Magic 
schools_clean <- gsub("[[:space:]]+NCAA", "", schools)
unique_schools_clean <- names(table(schools_clean))

# Replace Schools with Schools clean 
full_df$School <- schools_clean

# look again at a table of schools 
table(full_df$School)

# We still have some schools that don't have data for all of the years we want to study
# Let's remove all of the schools with incomplete data and put them in a separate data frame 
# for now. I'm not really sure what to do with them

idx_complete <- which(table(full_df$School) == 14)
schools_incomplete <- names(table(schools_clean))[-idx_complete]
df_incomplete <- full_df[which((full_df$School %in% schools_incomplete)), ]
write.csv(df_incomplete, file = "data/incomplete_data_clean.csv")

# Now let's subset our df for rows with complete data
df_complete <- full_df[which(!(full_df$School %in% schools_incomplete)), ]

# We have 322 unique schools with complete data from the 2003-2017 seasons
length(unique(df_complete$School))

# Write Clean Df


# Read in Clean DF 
df.clean <- read.csv("data/full_data_clean.csv")



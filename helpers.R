# Abstracted Helper Functions 
# Jess, Anna and Seth Project
# 11/29/19

# Read the data
read_data <- function() {
  df_names <- c("teams0405.csv", "teams0506.csv", "teams0607.csv",
                "teams0708.csv", "teams0809.csv", "teams0910.csv", 
                "teams1011.csv", "teams1112.csv", "teams1213.csv",
                "teams1314.csv", "teams1415.csv", "teams1516.csv",
                "teams1617.csv", "teams1718.csv")
  
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

# Check Dimensions
dim_checker <- function(df) {
  if (length(unique(df$School)) * length(unique(df$year)) == dim(df)[1]) {
    print("Dim Check Successful")
  } else {
    print("Dim Check Failed"); 
    print("# of unique schools:"); print(length(unique(df$School)));
    print("# of unique years:"); print(length(unique(df$year)));
    print("# of rows:"); print(dim(df)[1]);
  }
}

# Read in df and Add time
add_time <- function(df_str){
  df <- read.csv(paste("data/", df_str, sep = ""))
  df <- select (df,-c(X))
  df$time <- df$year - 2003
  return(df)
}

get_prop_df <- function(df) {
  drops_names <- c("year", "time", "W.L.", "SRS", "SOS","FTr","X3PAr","TS.", "TRB.", "AST.",  "BLK.", "eFG.", "TOV.", "FT.FGA", "FG.", "X3P.", "FT.")
  keep = df[drops_names]
  drop = df[ , !(names(df) %in% drops_names)]
  keep$id = 1:nrow(keep)
  drop$id = 1:nrow(drop)
  new_drop = drop[, 3:ncol(drop)]/(drop$G)
  prop_df = cbind(new_drop, keep)
  return(prop_df)
}

add_coach_change <- function(df) {
  schools <- c("Syracuse", "Duke", "Oakland", "Davidson", "Lafayette", "Michigan", 
               "Gonzaga", "Northwestern", "Yale", "Notre Dame", "Albany (NY)", 
               "Saint Mary's (CA)", "Villanova", "Florida State", "Baylor", 
               "Kansas", "North Carolina", "Western Michigan")
  schools <- paste0('^', schools, '$')
  pattern = paste(schools, collapse='|')
  df$same.coach = grepl(pattern, df$School)
  df[df$School == "Saint Mary's (CA)",]$same.coach = TRUE
  df[df$School == "Albany (NY)",]$same.coach = TRUE
  return(df)
}
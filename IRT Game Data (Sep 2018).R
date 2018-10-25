# IRT from current Prima game data

# Library Imports
library(readr) # CSV file I/O
library(dplyr) # Data wrangling
library(stringr)
source("./functions.R")

# Load raw data dump
df1 <- read.table("./Data/PRIMA_2018-01-09_2018-08-09.csv", fill = TRUE, header = TRUE, sep = ",") # 19143     5
df2 <- read.table("./Data/PRIMA_2018-09-09_2018-13-09.csv", fill = TRUE, header = TRUE, sep = ",") # 37882     5
df3 <- read.table("./Data/PRIMA_2018-13-09_2018-21-09.csv", fill = TRUE, header = TRUE, sep = ",") # 40871     5
df4 <- read.table("./Data/PRIMA_2018-21-09_2018-24-09.csv", fill = TRUE, header = TRUE, sep = ",") # 14885     5

df <- rbind(df1, df2, df3, df4) # 112781      5
ans <- clean_submit_answer_data(df) # 4728    9
head(ans)
# Select 1st attempts only
ans$attempt_count <- as.integer(ans$attempt_count)
ans <- ans[which(ans$attempt_count == 1) ,] # 3403   9
length(unique(ans$userId)) # 242 unique users
length(unique(paste0(ans$challenge_type, ans$problem_type))) # 41 unique challenge+problem types
# Create column for challenge+problem type
ans$chal_prob <- paste0(ans$challenge_type, ans$problem_type)

# There used to be a gameLevel field?
# How many unique challenge_type + problem_type pairs are there?

##### Check difference with old Beta data ####
ans.old <- read.table("./Data/submit_answer.csv", header=TRUE, sep=",", as.is = TRUE) #  15124 rows
# Select 1st attempts only
ans.old <- ans.old[which(ans$attempt_count == 1), ] # 8524 rows
ans.old <- ans.old[ order(ans.old[,2], ans.old[,5]), ] # Order by userId and gameLevel
# Find duplicate uid/game level
ans.old <- ans.old[!duplicated(ans.old[c("userId","gameLevel")]), ] # Remove duplicates (8407 rows)
# Create column for challenge+problem type
ans.old$chal_prob <- paste0(ans.old$challenge_type, ans.old$problem_type)
length(unique(ans.old$userId)) # 340 unique users
length(unique(ans.old$chal_prob)) # 37 unique challenge+problem types in Prima Beta
length(unique(ans.old$gameLevel)) # 59 levels in Prima Beta
betaLevelType <- ans.old[which(!duplicated(ans.old$gameLevel)), c("gameLevel", "chal_prob")]

setdiff(unique(ans.old$chal_prob), unique(ans$chal_prob))

        
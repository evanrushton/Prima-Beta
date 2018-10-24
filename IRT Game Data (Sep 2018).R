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

# There used to be a gameLevel field?
# How many unique challenge_type + problem_type pairs are there?

# Select 1st attempts only
ans$attempt_count <- as.integer(ans$attempt_count)
ans <- ans[which(ans$attempt_count == 1) ,] # 3403   9
length(unique(ans$userId)) # 242 unique users
length(unique(paste0(ans$challenge_type, ans$problem_type))) # 41 unique challenge+problem types
unique(paste0(ans$challenge_type, ans$problem_type))
    

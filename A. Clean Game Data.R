# Clean Prima Game Data for merge with pre/post scores

# Library Imports
library(readr) # CSV file I/O
library(dplyr) # Data wrangling

# Load raw data dump
PATH = "./Data/gameData.csv"
df <- read.table(PATH, fill = TRUE, header = TRUE, sep = ",") 
# sapply(df, function(y) sum(length(which(is.na(y)))))

# Select answer submissions
ans <- df[ which(df$actionName == "submit_answer"), ]

# ================== Clean actionData with regex ===========================
# \\"HEADER\\":\\"VALUE\\" -> VALUE
#
strip <- sapply(ans[, c(1,15:19)], function(y) gsub("[(\\\\\\\")({\\\\+)(}\\\\\")]", "", y)) # \\"HEADER\\":\\"VALUE\\" -> HEADER:VALUE
strip <- as.data.frame(strip)
pattern <- "((\\w*):(.*))" # \\1 <- HEADER:VALUE, \\2 <- HEADER, \\3 <- VALUE
# Set headers as captured header group
header <- "\\2"
headers <- lapply(strip[1,], function(y) gsub(pattern, header, y))
names(strip) <- headers; names(strip)[1] <- "X"
# Remove header from value field
replacement <- "\\3"
strip <- sapply(strip, function(y) gsub(pattern, replacement, y)) # HEADER:VALUE -> VALUE
strip <- as.data.frame(strip)
strip$X <- as.integer(strip$X)
strip$success <- as.integer(as.logical(strip$success)) # convert true/false to 1/0

# ================== Mutate cleaned dataframe by userID ===========================
ans <- left_join(ans[1:14], strip, by = "X") # Merge cleaned columns with answer submissions
grp <- ans %>% group_by(userId)# Group by UserID





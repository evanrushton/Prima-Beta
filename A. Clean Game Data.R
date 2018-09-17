# Clean Prima Game Data for merge with pre/post scores

# Library Imports
library(readr) # CSV file I/O
library(dplyr) # Data wrangling

# Load raw data dump
PATH = "./Data/primaBeta.csv"
df <- read.table(PATH, fill = TRUE, header = TRUE, sep = ",") 
#sapply(df, function(y) sum(length(which(is.na(y)))))

# Select rows by actionName
ans <- df[ which(df$actionName == "submit_answer"), ]
pen <-  df[ which(df$actionName == "resize_pen" | df$actionName == "submit_pen_answer"), ]

# Select desired cols
ans <- ans[c(1,6,11,5,12:18)]
pen <- pen[c(1,6,11,5,12,13,19,24)]

# ================== Clean ans actionData ===========================
pattern <- "((\\w*):(\\w*))" # \\1 <- HEADER:VALUE, \\2 <- HEADER, \\3 <- VALUE
# Set headers as captured header group
header <- "\\2"
headers <- lapply(ans[1, c(7:11)], function(y) gsub(pattern, header, y))
names(ans)[7:11] <- headers
names(ans)[c(7,8,11)] <- c("challenge_type", "problem_type", "latency")
# Remove header from value field
replacement <- "\\3"
ans[,7:11] <- sapply(ans[,7:11], function(y) gsub(pattern, replacement, y, perl=TRUE)) # HEADER:VALUE -> VALUE
ans <- as.data.frame(ans)
# Remove } and {"challenge_type"
ans[,11] <- sapply(ans[,11], function(y) gsub("}", "", y))
ans <- as.data.frame(ans)
ans[,7] <- sapply(ans[,7], function(y) gsub('\\{\\"challenge_type\\"', "", y))
ans <- as.data.frame(ans)
head(ans)
# Relabel improperly labeled levels
ans$gameLevel <- as.character(ans$gameLevel)
ans$gameLevel[ans$gameLevel == "2.05"] <- "2.03a"
ans$gameLevel[ans$gameLevel == "3.02b"] <- "3.01b"
ans$gameLevel[ans$gameLevel == "3.04b"] <- "3.03b"
ans$gameLevel[ans$gameLevel == "3.04d"] <- "3.03d"
ans$gameLevel <- as.factor(ans$gameLevel)

# ================== Clean pen actionData ===========================
# tweenquotes <- "([\"\'])(?:(?=(\\\\?))\\2.)(*?)\\1"
pattern <- "((\\w*):(\\w*))" # \\1 <- HEADER:VALUE, \\2 <- HEADER, \\3 <- VALUE
# Set headers 
names(pen)[7:8] <- c("prevPenDim", "success")
# Remove header from value field
replacement <- "\\3"
pen[,7:8] <- sapply(pen[,7:8], function(y) gsub(pattern, replacement, y, perl=TRUE)) # HEADER:VALUE -> VALUE
pen <- as.data.frame(pen)
# Remove }
pen[,8] <- sapply(pen[,8], function(y) gsub("}", "", y))
pen <- as.data.frame(pen)
head(pen)

# ================== Engineer Features for pen ===========================
# Make 2 copies of pen dimension to seperate x (creature), y (foodA), and z (foodB)
pen$pendim_y <- pen$prevPenDim
pen$pendim_z <- pen$prevPenDim
names(pen)[7] <- "pendim_x"
pen <- pen[c(1:7,9,10,8)]
pattern <- '\\"(\\d+)_(\\d+)_(\\d+)\\"' # Matches "1_1_0" or "22_66_0"
pen[,7] <- sapply(pen[,7], function(y) gsub(pattern, "\\1", y)); pen <- as.data.frame(pen) # capture dim_x
pen[,8] <- sapply(pen[,8], function(y) gsub(pattern, "\\2", y)); pen <- as.data.frame(pen) # capture dim_y
pen[,9] <- sapply(pen[,9], function(y) gsub(pattern, "\\3", y)); pen <- as.data.frame(pen) # capture dim_z

# ================== Convert types and write csv ===========================
# convert ans variables to proper type
ans$userId <- as.integer(as.character(ans$userId))
ans$sessionOrder <- as.integer(as.character(ans$sessionOrder))
ans$totalTimePlayed <- as.integer(as.character(ans$totalTimePlayed))
ans$success <- as.integer(as.logical(ans$success)) # convert true/false to 1/0
ans$attempt_count <- as.integer(as.character(ans$attempt_count))
ans$latency <- as.numeric(ans$latency)
ans$clientTimeStamp <- as.character(ans$clientTimeStamp) # strptime( ,"%m/%d/%Y %H:%M:%OS3")

# convert pen variables to proper type
pen$userId <- as.integer(as.character(pen$userId))
pen$sessionOrder <- as.integer(as.character(pen$sessionOrder))
pen$totalTimePlayed <- as.integer(as.character(pen$totalTimePlayed))
pen$success <- as.integer(as.logical(pen$success)) # convert true/false to 1/0
pen$pendim_x <- as.integer(pen$pendim_x)
pen$pendim_y <- as.integer(pen$pendim_y)
pen$pendim_z <- as.integer(pen$pendim_z)
pen$clientTimeStamp <- as.character(pen$clientTimeStamp) # strptime( ,"%m/%d/%Y %H:%M:%OS3")

# order by gameLevel, userId, clientTimeStamp
pen <- pen[ order(pen[,5], pen[,2], pen[,1]), ] # ordering timestamps "works" but is sorting characters rather than POSIXct

# write to csv
write_csv(pen, "./Data/resize_pen_submit_pen_answer.csv", na = "NA", append = FALSE, col_names = TRUE)
write_csv(ans, "./Data/submit_answer.csv", na = "NA", append = FALSE, col_names = TRUE)


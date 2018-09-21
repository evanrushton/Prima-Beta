# Clean original Surveygizmo .csv dumps for prea/preb forms and posta/postb forms

library(tidyverse)

# Read in the raw pre-test and post-test data  
prA <- read.delim("./Data/prA_clean.csv", header=TRUE, sep=",", as.is = TRUE) # 436  35
prB <- read.delim("./Data/prB.csv", header=TRUE, sep=",", as.is = TRUE) # 422  35
poA <- read.delim("./Data/poA.csv", header=TRUE, sep=",", as.is = TRUE) # 302  35
poB <- read.delim("./Data/poB.csv", header=TRUE, sep=",", as.is = TRUE) # 328  35

mylist <- list(prA, prB, poA, poB)

# ===== Change variable names =====
ChangeVarNames1 <- function(mydata, cols) {
  # Remove questions from the item names
  pattern <- '(X.\\w+.\\w+.\\w+[._]\\w+)(\\S+)' # \\1 <- X.21.ND.NUR_L  \\2 <- .....6.trains.carry.a.total.of.18.cars..How.many.cars.will.8.trains.carry.... 
  level <- "\\1"
  names(mydata)[cols] <- lapply(names(mydata)[cols], function(y) gsub(pattern, level, y))
  # Shorten long variable names
  names(mydata)[c(6,7,9,10,11)] <- c("first", "last", "g","screen", "uid")
  return(mydata)
}
# Apply to all dfs
mylist <- lapply(mylist, function(x) ChangeVarNames1(x, 12:ncol(x)))

ChangeVarNames2 <- function(mydata, cols) {
  # Remove X. from the item names
  pattern <- '(X.)(\\w+.\\w+.\\w+[._]\\w+)' # \\1 <- X.  \\2 <- 21.ND.NUR_L 
  level <- "\\2"
  names(mydata)[cols] <- lapply(names(mydata)[cols], function(y) gsub(pattern, level, y))
  # Further shorten long variable names
  names(mydata)[c(1,2,3,8)] <- c("id", "startTime", "date","teacher")
  return(mydata)
}
# Apply to all dfs
mylist <- lapply(mylist, function(x) ChangeVarNames2(x, 12:ncol(x)))

# ===== Combine Pre A/B and Post A/B =====
# Forms A and B are the same 24 items in different order, just stack them
pr <- rbind(mylist[[1]], mylist[[2]]) # 858  35
po <- rbind(mylist[[3]], mylist[[4]]) # 630  35
# Correct naming error on pr
names(pr)[c(12,14)] <- c("1.MD.UR.pr","3.MD.UR_L")
# Select 6 Linking Items
link_items <- c("3.MD.UR_L","12.MD.NUR_L", "16.MS.NUR_L", "21.ND.NUR_L", "34.MT.UR_L", "40.MT.NUR_L")
nonlink_pr <- setdiff(names(pr)[12:ncol(pr)], link_items)
nonlink_po <- setdiff(names(po)[12:ncol(po)], link_items)
# Stack pr/po
prlink <- pr[,which(names(pr) %in% setdiff(names(pr),nonlink_pr))]
polink <- po[,which(names(po) %in% setdiff(names(po),nonlink_po))]
df <- rbind(prlink,polink)
# Check Responses
table(df$`12.MD.NUR_L`) # Need to clean some of the responses
# Isolate digits
df[,c(12:ncol(df))] <- lapply(df[,c(12:ncol(df))], function(x) gsub('\\D*(\\d+)\\D*', '\\1', x, perl=T))
# Count each response
tablelist <- lapply(df[,link_items], function(responses) table(responses))
# Most common responses
sapply(tablelist, function(x) x[which(x>10)])


# IRT of Game Response Data
# Authors: Evan Rushton, Seth Corrigan
# Created: 09-17-18

library("eRm")
library("dplyr")
library("reshape2")

ans <- read.table("./Data/submit_answer.csv", header=TRUE, sep=",", as.is = TRUE) #  15124 rows
# Select 1st attempts only
ans <- ans[which(ans$attempt_count == 1), ] # 8524 rows
ans <- ans[ order(ans[,2], ans[,5]), ] # Order by userId and gameLevel
# Find duplicate uid/game level
ans <- ans[!duplicated(ans[c("userId","gameLevel")]), ] # Remove duplicates (8407 rows)
# Remove hand-feeding challenges T2.01 T1.01 T2.01.REPLACE (adult ram, baby ram, 3 baby ram)
ans <- setdiff(ans, ans[which(ans$gameLevel %in% c("T2.01", "T1.01", "T2.01-REPLACE")), ]) # 7325 rows

# Select desired vars
resp <- ans[, c(2,5,8,9)]

# Pivot answer submissions to make response vectors (concatenate problem_type to gameLevel)
resp <- dcast(resp, userId ~ gameLevel + problem_type, fill = 3, fun.aggregate = mean, value.var = "success") # 689
# Remove quotes //"
names(resp) <- lapply(names(resp), function(y) gsub('\\"', "", y))
# Replace non-attempts with NA and keep 0 for first attempt fails
resp <- data.frame(lapply(resp, function(y) gsub("^3$", NA, y)), check.names = FALSE)
sapply(resp, function(y) sum(length(which(!is.na(y))))) # Attempt counts (item)
person_attempts <- sapply(1:nrow(resp), function(y) sum(length(which(!is.na(resp[y,2:ncol(resp)]))))) # Attempt counts (person)
resp <- resp[which(person_attempts > 1) ,] # 658 59
resp <- data.frame(lapply(resp, function(x) as.numeric(as.character(x))), check.names = FALSE)

df <- resp[-c(1)]
core_levels <- c("T1.02a_2UR1MC", "T1.03_2UR1MC" , "T1.05_2UR1MF", "T1.04_2UR1MC", "T1.07a_2US_intro_baby_unifox", "1.05b_2US1NC", "1.09a_2US2NF", "1.08_2US1NC", "1.07a_2US2MF", "1.06_2UR1MF", "2.01c_3UR1MC", "2.06b_3UR2MTb", "2.05a_3UR2MTa", "2.04b_3UR2MF", "2.03a_3UR2MF", "T3.01a_2BR_intro_baby_bird", "3.01b_2BR1MC", "3.03b_2BR2MF", "3.03d_2BR2MF", "T4.01a_3BR_intro_adult_bird", "T4.01b_3BR1MC" , "T4.02_3US_intro_adult_unifox", "T4.03a_3US1MC", "4.07b_3US1NC", "4.1_3BR3MF", "4.16_3US2NF", "4.17_3US2NTa", "4.13_3BR2MTb") #28 levels

df <- df[which(names(df) %in% core_levels)] # 658  28
person_attempts <- sapply(1:nrow(df), function(y) sum(length(which(!is.na(df[y,1:ncol(df)]))))) # Attempt counts (person)
df <- df[which(person_attempts > 10) ,] # 230 28

#Rasch
res.rasch <- RM(df)
#Person Parameters
pres.rasch <- person.parameter(res.rasch)

#Andersen’s LR-test for goodness-of-fit with mean split criterion
lrres.rasch <- LRtest(res.rasch, splitcr = "mean")
lrres.rasch
summary(lrres.rasch)

plotGOF(lrres.rasch, beta.subset = c(13, 3, 18, 6, 1, 20) , tlab = "item", #c(14, 5, 15, 3, 1) 
        conf = list(ia = FALSE, col = "green", lty = "dotted"))
#Wright Map
plotPImap(res.rasch, sorted = TRUE, cex.gen = 0.4)

#Item Fit
#For easy description of the fit statistics see: http://www.rasch.org/rmt/rmt82a.htm
itemfit(pres.rasch)


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

# Select desired vars
resp <- ans[, c(2,5,8,9)]

# Pivot answer submissions to make response vectors (concatenate problem_type to gameLevel)
resp <- dcast(resp, userId ~ gameLevel + problem_type, fill = 3, fun.aggregate = mean, value.var = "success")
# Remove quotes //"
names(resp) <- lapply(names(resp), function(y) gsub('\\"', "", y))
# Replace non-attempts with NA and keep 0 for first attempt fails
resp <- data.frame(lapply(resp, function(y) gsub("^3$", NA, y)), check.names = FALSE)
sapply(resp, function(y) sum(length(which(!is.na(y))))) # Attempt counts
# Remove hand-feeding challenges T2.01 T1.01 T2.01.REPLACE (adult ram, baby ram, 3 baby ram)
resp <- resp[ , setdiff(names(resp), c("T2.01_3UR_intro_adult_rammus", "T1.01_2UR_intro_baby_ram", "T2.01-REPLACE_3UR_intro_adult_rammus-REPLACE"))]
resp <- data.frame(lapply(resp, function(x) as.numeric(as.character(x))), check.names = FALSE)

df <- resp[-c(1)]
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


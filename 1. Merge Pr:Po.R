# Merge Pre and Post scores for descriptives
# Write mergeSurvey.csv (N = 343)

library(pastecs) # For stat.desc descriptives
library(readr) # For CSV file I/O

# Read in the pre-test and post-test data
pr <- read.table("./Data/prsurvey.csv", header=TRUE, sep=",", as.is = TRUE) # [1] 594  29
po <- read.table("./Data/posurvey.csv", header=TRUE, sep=",", as.is = TRUE) # [1] 468  27

sapply(pr, function(y) sum(length(which(is.na(y))))) # 21 pre response vectors have no uid

# Join/merge the pre-test and post test files in wide format
df <- merge(pr, po, by = "uid")

head(df)
dim(pr); dim(po); dim(df) # only 343 kids completed both pre AND post

# Check Merge with sample vector of kids vec
vec <- c("8535", "8466", "8346", "8930", "9072")
pr[pr$uid %in% vec,]; po[po$uid %in% vec,]; df[df$uid %in% vec,]; # looks to have merged properly

# Write merge to file
write_csv(df, "./Data/mergeSurvey.csv", na = "NA", append = FALSE, col_names = TRUE)

# Descriptives
summary(df)

scores <- df[, !(names(df) %in% c("grd", "pr_prcnt", "g", "tchr", "uid"))]
stat.desc(scores, basic=F)

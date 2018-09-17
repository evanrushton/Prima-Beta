# Merge game response vectors with pr/po data for correlation stats

# Library Imports
library(tidyverse)
library(reshape2)

# Load in game answer submissions
ans <- read.table("./Data/submit_answer.csv", fill = TRUE, header = TRUE, sep = ",")
# Relabel improperly labeled levels
ans$gameLevel <- as.character(ans$gameLevel)
ans$gameLevel[ans$gameLevel == "2.05"] <- "2.03a"
ans$gameLevel[ans$gameLevel == "3.02b"] <- "3.01b"
ans$gameLevel[ans$gameLevel == "3.04b"] <- "3.03b"
ans$gameLevel[ans$gameLevel == "3.04d"] <- "3.03d"

# Create gameLevel labels
level_labels <- ans[, c(5,7,8)]
level_labels <- level_labels[!duplicated(level_labels$gameLevel), ] 
level_labels <- level_labels[ order(level_labels[,1]), ]
all_levels <- as.character(level_labels$gameLevel)
# Remove hand-feeding challenges T2.01 T1.01 T2.01.REPLACE (adult ram, baby ram, 3 baby ram)
all_levels <- setdiff(all_levels, c("T2.01", "T1.01", "T2.01-REPLACE"))
core_levels <- c("T1.02a", "T1.03", "T1.05", "T1.04", "T1.07a", "1.05b", "1.09a", "1.08", "1.07a", "1.06", "2.01c", "2.06b", "2.05a", "2.04b", "2.03a", "T3.01a", "3.01b", "3.03b", "3.03d", "T4.01a", "T4.01b", "T4.02", "T4.03a", "4.07b", "4.1", "4.16", "4.17", "4.13")
review_levels <- c("1.01a", "1.01b", "1.02b", "1.02c", "1.03a", "1.03c", "1.04a", "1.05a", "1.05c", "1.07b", "2.01a", "2.01b", "2.02a", "2.02c", "2.03b", "2.04a", "2.06a", "3.01a", "3.01c", "3.03a", "4.02a", "4.03a", "4.03b", "4.03c", "4.04a", "4.07b", "4.08", "4.11", "4.15")
non_tutorial_core <- c("1.05b", "1.09a", "1.08", "1.07a", "1.06", "2.01c", "2.06b", "2.05a", "2.04b", "2.03a", "3.01b", "3.03b", "3.03d", "4.07b", "4.1", "4.16", "4.17", "4.13")
tutorial_core <- c("T1.02a", "T1.03", "T1.05", "T1.04", "T1.07a", "T3.01a", "T4.01a", "T4.01b", "T4.02", "T4.03a")
core1_levels <- c("T1.02a", "T1.03", "T1.05", "T1.04", "T1.07a", "1.05b", "1.09a", "1.08", "1.07a", "1.06")
review1_levels <- c("1.01a", "1.01b", "1.02b", "1.02c", "1.03a", "1.03c", "1.04a", "1.05a", "1.05c", "1.07b")
core2_levels <- c("2.01c", "2.06b", "2.05a", "2.04b", "2.03a")
review2_levels <- c("2.01a", "2.01b", "2.02a", "2.02c", "2.03b", "2.04a", "2.06a")
core3_levels <- c("T3.01a", "3.01b", "3.03b", "3.03d")
review3_levels <- c("3.01a", "3.01c", "3.03a")
core4_levels <- c("T4.01a", "T4.01b", "T4.02", "T4.03a", "4.07b", "4.1", "4.16", "4.17", "4.13")
review4_levels <- c("4.02a", "4.03a", "4.03b", "4.03c", "4.04a", "4.07b", "4.08", "4.11", "4.15")

# Select 1st attempts only
ans <- ans[which(ans$attempt_count == 1), ]
ans <- ans[ order(ans[,2], ans[,5]), ] # 8524 rows
# Find duplicate uid/game level
ans <- ans[!duplicated(ans[c("userId","gameLevel")]), ] # Remove duplicates (8407 rows)

# View gameLevel sequences
ans[order(ans[,2], ans[,1]), c("userId", "clientTimeStamp", "gameLevel", "success")]

# Pivot answer submissions to make response vectors
resp <- ans[, c(2,5,9)]
resp <- dcast(resp, userId ~ gameLevel, fill = 3, fun.aggregate = mean)
# Replace non-attempts with NA and keep 0 for first attempt fails
resp <- data.frame(lapply(resp, function(y) gsub("^3$", NA, y)), check.names = FALSE)
sapply(resp, function(y) sum(length(which(!is.na(y))))) # Attempt counts
# Remove hand-feeding challenges T2.01 T1.01 T2.01.REPLACE (adult ram, baby ram, 3 baby ram)
resp <- resp[ , setdiff(names(resp), c("T2.01", "T1.01", "T2.01-REPLACE"))]
resp <- data.frame(lapply(resp, function(x) as.numeric(as.character(x))), check.names = FALSE)

# Find total score across game data response vectors (respTotals)
resp %>%select(which(names(resp) %in% all_levels)) %>% rowSums(na.rm=TRUE) -> resp$allTot
# Since review levels are not seen by all players, subset into core levels
resp %>% select(which(names(resp) %in% core_levels)) %>% rowSums(na.rm=TRUE) -> resp$coreTot
# to see how review levels compare, look at them independently
resp %>% select(which(names(resp) %in% review_levels)) %>% rowSums(na.rm=TRUE) -> resp$reviewTot
# Check tutorial vs non-tutorial levels
resp %>% select(which(names(resp) %in% non_tutorial_core)) %>% rowSums(na.rm=TRUE) -> resp$nonTutorialCoreTot
resp %>% select(which(names(resp) %in% tutorial_core)) %>% rowSums(na.rm=TRUE) -> resp$tutorialCoreTot

# Since game is teaching along the way, subset game performance into levels (respLevels) {L1, L2, L3, L4} {core, core1, core2, core3, core4} {review1, review2, review3, review4}
resp %>% select(which(names(resp) %in% c(review1_levels, core1_levels))) %>% rowSums(na.rm=TRUE) -> resp$L1Tot
resp %>% select(which(names(resp) %in% core1_levels)) %>% rowSums(na.rm=TRUE) -> resp$core1Tot
resp %>% select(which(names(resp) %in% review1_levels)) %>% rowSums(na.rm=TRUE) -> resp$review1Tot
resp %>% select(which(names(resp) %in% c(review2_levels, core2_levels))) %>% rowSums(na.rm=TRUE) -> resp$L2Tot
resp %>% select(which(names(resp) %in% core2_levels)) %>% rowSums(na.rm=TRUE) -> resp$core2Tot
resp %>% select(which(names(resp) %in% review2_levels)) %>% rowSums(na.rm=TRUE) -> resp$review2Tot
resp %>% select(which(names(resp) %in% c(review3_levels, core3_levels))) %>% rowSums(na.rm=TRUE) -> resp$L3Tot
resp %>% select(which(names(resp) %in% core3_levels)) %>% rowSums(na.rm=TRUE) -> resp$core3Tot
resp %>% select(which(names(resp) %in% review3_levels)) %>% rowSums(na.rm=TRUE) -> resp$review3Tot
resp %>% select(which(names(resp) %in% c(review4_levels, core4_levels))) %>% rowSums(na.rm=TRUE) -> resp$L4Tot
resp %>% select(which(names(resp) %in% core4_levels)) %>% rowSums(na.rm=TRUE) -> resp$core4Tot
resp %>% select(which(names(resp) %in% review4_levels)) %>% rowSums(na.rm=TRUE) -> resp$review4Tot

# Check level combinations after level 1 (respCombos) {2-3, 2-4, 3-4} 
resp <- mutate(resp, L23Tot = L2Tot + L3Tot)
resp <- mutate(resp, core23Tot = core2Tot + core3Tot)
resp <- mutate(resp, review23Tot = review2Tot + review3Tot)
resp <- mutate(resp, L24Tot = L2Tot + L3Tot + L4Tot)
resp <- mutate(resp, core24Tot = core2Tot + core3Tot + core4Tot)
resp <- mutate(resp, review24Tot = review2Tot + review3Tot + review4Tot)
resp <- mutate(resp, L34Tot = L3Tot + L4Tot)
resp <- mutate(resp, core34Tot = core3Tot + core4Tot)
resp <- mutate(resp, review34Tot = review3Tot + review4Tot)

# Form matrices of different response vectors to compare correlations
names(resp)[1] <- "uid"
respTotals <- resp[, c("uid", "allTot", "coreTot", "reviewTot", "nonTutorialCoreTot", "tutorialCoreTot")]
respLevels <- resp[, c("uid", "L1Tot", "core1Tot", "review1Tot", "L2Tot", "core2Tot","review2Tot", "L3Tot", "core3Tot", "review3Tot", "L4Tot", "core4Tot", "review4Tot")]
respCombos <- resp[, c("uid", "L23Tot", "core23Tot", "review23Tot", "L24Tot", "core24Tot","review24Tot", "L34Tot", "core34Tot", "review34Tot")]

# Load in pr/po scores and ability estimates (thetas)
theta <- read.table("./Data/prpotheta.csv", fill = TRUE, header = TRUE, sep = ",") 
theta <- cbind(theta, diff=theta$po_est-theta$pr_est) # add column with difference between ability estimates (po - pr)

# Merge pr/po/resp totals by UID
totals <- merge(theta, respTotals, by = "uid")
levels <- merge(theta, respLevels, by = "uid")
combos <- merge(theta, respCombos, by = "uid")

tot <- totals[, 4:ncol(totals)] # Largest positive correlation with nonTutorial levels -> nonTutorialCoreTot 0.27711696 0.2877823 0.28520689 0.26184614
lev <- levels[, 4:ncol(levels)] # first review levels negatively correlate with survey performance review1Tot -0.30359333 -0.28300671 -0.28809288
com <- combos[, 4:ncol(combos)]
# Pearson correlation
cor(tot,tot)[,c(1:5)] 
cor(lev,lev)[,c(1:5)] 
cor(com,com)[,c(1:5)] 
# pr/po data is positively correlated. 
# Game data total response vectors (respTot) are weakly correlated to survey data (0.15 po, 0.18 pr)
# Core levels 2-4 (core24Tot) have a positive correlation (0.37 po, 0.407 pr)
# Survey Slopes (diff) have a negative correlation with pre and a positive correlation with post (-0.266 pr, 0.389 po)


# Load in game response vectors from Seth's data cleaning
resp_val <- read.table("./Data/gameRespVec.csv", fill = TRUE, header = TRUE, sep = ",")
respTot_val <- resp_val[, c(1,ncol(resp_val))] # Number of correct 1st attempts per player
df_val <- merge(theta, respTot_val, by = "uid")
num_val <- df_val[4:8]
# Pearson correlation
cor(num_val,num_val) # pr/po data is positively correlated.
# 0.1735607, slight positive correlation Grand.Total with po_est

# Correlation between in-game performance and pr/po ability estimates 
gameperf <- totals$allTot
thetapo <- totals$po_est
diff <- totals$diff
core24 <- combos$core24Tot
cor(core24,thetapo) # 0.1793418 slight positive correlation

# 1:1 Plot of Game Performance vs Post Ability Estimates
plot(gameperf, thetapo,
     pch = 16,
     xlab="Game Performance",
     ylab="Ability Post")
abline(-4,2/5, col="blue", lwd=2)

plot(core24, thetapo, # Since core24Tot has a better correlation with thetapo, may be a more telling measure
     pch = 16,
     xlab="Core Levels 2-4 Performance",
     ylab="Ability Post")
abline(lm(thetapo ~ core24), col="red", lwd=2)
abline(-4,2/3, col="blue", lwd=2)

scatter <- qplot(core24, thetapo)  + 
  scale_x_continuous(limits=c(min(core24),max(core24))) + 
  scale_y_continuous(limits=c(min(thetapo),max(thetapo))) + 
  geom_rug(col=rgb(.5,0,0,alpha=.2))
scatter
abline(-4,2/3, col="blue", lwd=2)

kdp <- ggplot(totals, aes(x = totals$allTot, y = totals$po_est)) +
  geom_point(aes(colour = totals$tchr)) +
  xlim(-2, 24) +
  ylim(-6, 6)
kdp + geom_density_2d(aes(colour = totals$tchr)) # aes(colour = df$tchr)


# Since progress variable contains various types, add categories for each problem type

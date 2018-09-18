# Merge game response vectors with pr/po data for correlation stats

# Library Imports
library(tidyverse)
library(reshape2)

# Load in game answer submissions
ans <- read.table("./Data/submit_answer.csv", fill = TRUE, header = TRUE, sep = ",")
# Load in pr/po scores and ability estimates (thetas)
theta <- read.table("./Data/prpotheta.csv", fill = TRUE, header = TRUE, sep = ",") 
theta <- cbind(theta, diff=theta$po_est-theta$pr_est) # add column with difference between ability estimates (po - pr)

# Select 1st attempts only
ans <- ans[which(ans$attempt_count == 1), ] # 8524 rows
ans <- ans[ order(ans[,2], ans[,5]), ] # Order by userId and gameLevel
# Find duplicate uid/game level
ans <- ans[!duplicated(ans[c("userId","gameLevel")]), ] # Remove duplicates (8407 rows)

# View gameLevel sequences
ans[order(ans[,2], ans[,1]), c("userId", "clientTimeStamp", "gameLevel", "success")]

# Create gameLevel labels
level_labels <- ans[, c(5,7,8)]
level_labels <- level_labels[!duplicated(level_labels$gameLevel), ] 
level_labels <- level_labels[ order(level_labels[,1]), ]
all_levels <- as.character(level_labels$gameLevel)
# Remove hand-feeding challenges T2.01 T1.01 T2.01.REPLACE (adult ram, baby ram, 3 baby ram)
all_levels <- setdiff(all_levels, c("T2.01", "T1.01", "T2.01-REPLACE"))
core_levels <- c("T1.02a", "T1.03", "T1.05", "T1.04", "T1.07a", "1.05b", "1.09a", "1.08", "1.07a", "1.06", "2.01c", "2.06b", "2.05a", "2.04b", "2.03a", "T3.01a", "3.01b", "3.03b", "3.03d", "T4.01a", "T4.01b", "T4.02", "T4.03a", "4.07b", "4.1", "4.16", "4.17", "4.13") #28 levels
review_levels <- c("1.01a", "1.01b", "1.02b", "1.02c", "1.03a", "1.03c", "1.04a", "1.05a", "1.05c", "1.07b", "2.01a", "2.01b", "2.02a", "2.02c", "2.03b", "2.04a", "2.06a", "3.01a", "3.01c", "3.03a", "4.02a", "4.03a", "4.03b", "4.03c", "4.04a", "4.07b", "4.08", "4.11", "4.15") #29 levels
non_tutorial_core <- c("1.05b", "1.09a", "1.08", "1.07a", "1.06", "2.01c", "2.06b", "2.05a", "2.04b", "2.03a", "3.01b", "3.03b", "3.03d", "4.07b", "4.1", "4.16", "4.17", "4.13") #18 levels
tutorial_core <- c("T1.02a", "T1.03", "T1.05", "T1.04", "T1.07a", "T3.01a", "T4.01a", "T4.01b", "T4.02", "T4.03a") #10 levels
totals_list <- list(all=all_levels, core=core_levels, review=review_levels, ntc=non_tutorial_core, tc=tutorial_core)
core1_levels <- c("T1.02a", "T1.03", "T1.05", "T1.04", "T1.07a", "1.05b", "1.09a", "1.08", "1.07a", "1.06") #10 levels
review1_levels <- c("1.01a", "1.01b", "1.02b", "1.02c", "1.03a", "1.03c", "1.04a", "1.05a", "1.05c", "1.07b") #10 levels
ntc1_levels <- c("1.05b", "1.09a", "1.08", "1.07a", "1.06") #5 levels
core2_levels <- c("2.01c", "2.06b", "2.05a", "2.04b", "2.03a") #5 levels
review2_levels <- c("2.01a", "2.01b", "2.02a", "2.02c", "2.03b", "2.04a", "2.06a") #7 levels
ntc2_levels <- c("2.01c", "2.06b", "2.05a", "2.04b", "2.03a") #5 levels
core3_levels <- c("T3.01a", "3.01b", "3.03b", "3.03d") #4 levels
review3_levels <- c("3.01a", "3.01c", "3.03a") #3 levels
ntc3_levels <- c("3.01b", "3.03b", "3.03d") #3 levels
core4_levels <- c("T4.01a", "T4.01b", "T4.02", "T4.03a", "4.07b", "4.1", "4.16", "4.17", "4.13") #9 levels
review4_levels <- c("4.02a", "4.03a", "4.03b", "4.03c", "4.04a", "4.07b", "4.08", "4.11", "4.15") #9 levels
ntc4_levels <- c("4.07b", "4.1", "4.16", "4.17", "4.13") #5 levels
levels_list <- list(core1=core1_levels, review1=review1_levels, ntc1=ntc1_levels, core2=core2_levels, review2=review2_levels, core3=core3_levels, review3=review3_levels, ntc3=ntc3_levels, core4=core4_levels, review4=review4_levels, ntc4=ntc4_levels)

# Pivot answer submissions to make response vectors
resp <- ans[, c(2,5,9)]
resp <- dcast(resp, userId ~ gameLevel, fill = 3, fun.aggregate = mean)
# Replace non-attempts with NA and keep 0 for first attempt fails
resp <- data.frame(lapply(resp, function(y) gsub("^3$", NA, y)), check.names = FALSE)
sapply(resp, function(y) sum((!is.na(y)))) # Attempt counts
# Remove hand-feeding challenges T2.01 T1.01 T2.01.REPLACE (adult ram, baby ram, 3 baby ram)
resp <- resp[ , setdiff(names(resp), c("T2.01", "T1.01", "T2.01-REPLACE"))]
resp <- data.frame(lapply(resp, function(x) as.numeric(as.character(x))), check.names = FALSE)
names(resp)[1] <- "uid" # change name for later merge with theta
cols <- ncol(resp)

resp$completed <- sapply(1:nrow(resp), function(x) sum(!is.na(resp[x,2:cols])))

# Trying to count completed amount for each list of levels in one sapply statement?
# Count number of challenges completed (0 or 1)
CountComplete <- function(levelslist) {
  for (i in 1:length(levelslist)) {
    name <- paste0(names(levelslist)[i], "complete")
    return(name, sapply(1:nrow(resp), function(x) sum(!is.na(resp[x,levelslist[i]])))) 
  }
}
resp$CountComplete(totals_list)
sapply(1:nrow(resp), function(x,y) resp %>% select(which(names(resp) %in% y)) %>% sum(!is.na(resp[x,y]))) -> resp$
sapply(totals_list, function(y) resp %>% select(which(names(resp) %in% y)) %>% rowSums(na.rm=TRUE)) -> resp$y

# ===== All challenges completed (N = 697) =====
# Find total score across game data response vectors (totals_list)
resp %>% select(which(names(resp) %in% all_levels)) %>% rowSums(na.rm=TRUE) -> resp$allTot
resp$allTot <- resp$allTot / resp$completed

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
resp %>% select(which(names(resp) %in% ntc1_levels)) %>% rowSums(na.rm=TRUE) -> resp$ntc1Tot
resp %>% select(which(names(resp) %in% c(review2_levels, core2_levels))) %>% rowSums(na.rm=TRUE) -> resp$L2Tot
resp %>% select(which(names(resp) %in% core2_levels)) %>% rowSums(na.rm=TRUE) -> resp$core2Tot
resp %>% select(which(names(resp) %in% review2_levels)) %>% rowSums(na.rm=TRUE) -> resp$review2Tot
resp %>% select(which(names(resp) %in% ntc2_levels)) %>% rowSums(na.rm=TRUE) -> resp$ntc2Tot
resp %>% select(which(names(resp) %in% c(review3_levels, core3_levels))) %>% rowSums(na.rm=TRUE) -> resp$L3Tot
resp %>% select(which(names(resp) %in% core3_levels)) %>% rowSums(na.rm=TRUE) -> resp$core3Tot
resp %>% select(which(names(resp) %in% review3_levels)) %>% rowSums(na.rm=TRUE) -> resp$review3Tot
resp %>% select(which(names(resp) %in% ntc3_levels)) %>% rowSums(na.rm=TRUE) -> resp$ntc3Tot
resp %>% select(which(names(resp) %in% c(review4_levels, core4_levels))) %>% rowSums(na.rm=TRUE) -> resp$L4Tot
resp %>% select(which(names(resp) %in% core4_levels)) %>% rowSums(na.rm=TRUE) -> resp$core4Tot
resp %>% select(which(names(resp) %in% review4_levels)) %>% rowSums(na.rm=TRUE) -> resp$review4Tot
resp %>% select(which(names(resp) %in% ntc4_levels)) %>% rowSums(na.rm=TRUE) -> resp$ntc4Tot

# Check level combinations after level 1 (respCombos) {2-3, 2-4, 3-4} 
resp <- mutate(resp, L23Tot = L2Tot + L3Tot)
resp <- mutate(resp, core23Tot = core2Tot + core3Tot)
resp <- mutate(resp, review23Tot = review2Tot + review3Tot)
resp <- mutate(resp, ntc23Tot = ntc2Tot + ntc3Tot)
resp <- mutate(resp, L24Tot = L2Tot + L3Tot + L4Tot)
resp <- mutate(resp, core24Tot = core2Tot + core3Tot + core4Tot)
resp <- mutate(resp, review24Tot = review2Tot + review3Tot + review4Tot)
resp <- mutate(resp, ntc24Tot = ntc2Tot + ntc3Tot + ntc4Tot)
resp <- mutate(resp, L34Tot = L3Tot + L4Tot)
resp <- mutate(resp, core34Tot = core3Tot + core4Tot)
resp <- mutate(resp, review34Tot = review3Tot + review4Tot)
resp <- mutate(resp, ntc34Tot = ntc3Tot + ntc4Tot)

# Turn total vars into ratios to see if there is a correlation difference?
resp <- mutate(resp, allTotratio = allTot / 58) 
resp <- mutate(resp, coreTotratio = coreTot / 28) 
resp <- mutate(resp, reviewTotratio = reviewTot / 29)
resp <- mutate(resp, ntcratio = nonTutorialCoreTot / 18) 
resp <- mutate(resp, tutorialCoreratio = tutorialCoreTot / 10) 

#temporary test
respTotals <- resp[, c("uid", "allTot")]

# Form matrices of different response vectors to compare correlations
respTotals <- resp[, c("uid", "allTot", "coreTot", "reviewTot", "nonTutorialCoreTot", "tutorialCoreTot", "allTotratio", "coreTotratio", "reviewTotratio", "ntcratio", "tutorialCoreratio")]
respLevels <- resp[, c("uid", "L1Tot", "core1Tot", "review1Tot", "ntc1Tot", "L2Tot", "core2Tot","review2Tot", "ntc2Tot", "L3Tot", "core3Tot", "review3Tot", "ntc3Tot", "L4Tot", "core4Tot", "review4Tot", "ntc4Tot")]
respCombos <- resp[, c("uid", "L23Tot", "core23Tot", "review23Tot", "ntc23Tot", "L24Tot", "core24Tot","review24Tot", "ntc24Tot", "L34Tot", "core34Tot", "review34Tot", "ntc34Tot")]

# Merge pr/po/resp totals by UID (N = 266)
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

model <- lm(combos$po_est ~ combos$L24Tot)
anova(model)
summary(model)
plot(model)

# ===== 10 or more challenges (N = 421)  =====
tenOrMore <- resp[which(resp$completed >= 10),]

# Find total score across game data response vectors (respTotals)
tenOrMore %>%select(which(names(tenOrMore) %in% all_levels)) %>% rowSums(na.rm=TRUE) -> tenOrMore$allTot
# Since review levels are not seen by all players, subset into core levels
tenOrMore %>% select(which(names(tenOrMore) %in% core_levels)) %>% rowSums(na.rm=TRUE) -> tenOrMore$coreTot
tenOrMore %>% select(which(names(tenOrMore) %in% non_tutorial_core)) %>% rowSums(na.rm=TRUE) -> tenOrMore$nonTutorialCoreTot
tenOrMore %>% select(which(names(tenOrMore) %in% c(review2_levels, core2_levels))) %>% rowSums(na.rm=TRUE) -> tenOrMore$L2Tot
tenOrMore %>% select(which(names(tenOrMore) %in% core2_levels)) %>% rowSums(na.rm=TRUE) -> tenOrMore$core2Tot
tenOrMore %>% select(which(names(tenOrMore) %in% ntc2_levels)) %>% rowSums(na.rm=TRUE) -> tenOrMore$ntc2Tot
tenOrMore %>% select(which(names(tenOrMore) %in% c(review3_levels, core3_levels))) %>% rowSums(na.rm=TRUE) -> tenOrMore$L3Tot
tenOrMore %>% select(which(names(tenOrMore) %in% core3_levels)) %>% rowSums(na.rm=TRUE) -> tenOrMore$core3Tot
tenOrMore %>% select(which(names(tenOrMore) %in% ntc3_levels)) %>% rowSums(na.rm=TRUE) -> tenOrMore$ntc3Tot
tenOrMore %>% select(which(names(tenOrMore) %in% c(review4_levels, core4_levels))) %>% rowSums(na.rm=TRUE) -> tenOrMore$L4Tot
tenOrMore %>% select(which(names(tenOrMore) %in% core4_levels)) %>% rowSums(na.rm=TRUE) -> tenOrMore$core4Tot
tenOrMore %>% select(which(names(tenOrMore) %in% ntc4_levels)) %>% rowSums(na.rm=TRUE) -> tenOrMore$ntc4Tot
tenOrMore <- mutate(tenOrMore, L23Tot = L2Tot + L3Tot)
tenOrMore <- mutate(tenOrMore, core23Tot = core2Tot + core3Tot)
tenOrMore <- mutate(tenOrMore, ntc23Tot = ntc2Tot + ntc3Tot)
tenOrMore <- mutate(tenOrMore, L24Tot = L2Tot + L3Tot + L4Tot)
tenOrMore <- mutate(tenOrMore, core24Tot = core2Tot + core3Tot + core4Tot)
tenOrMore <- mutate(tenOrMore, ntc24Tot = ntc2Tot + ntc3Tot + ntc4Tot)
tenOrMore <- mutate(tenOrMore, L34Tot = L3Tot + L4Tot)
tenOrMore <- mutate(tenOrMore, core34Tot = core3Tot + core4Tot)
tenOrMore <- mutate(tenOrMore, ntc34Tot = ntc3Tot + ntc4Tot)

# Form matrices of different response vectors to compare correlations
tenOrMoreTotals <- tenOrMore[, c("uid", "allTot", "coreTot", "nonTutorialCoreTot")]
tenOrMoreLevels <- tenOrMore[, c("uid", "L2Tot", "core2Tot", "ntc2Tot", "L3Tot", "core3Tot", "ntc3Tot", "L4Tot", "core4Tot", "ntc4Tot")]
tenOrMoreCombos <- tenOrMore[, c("uid", "L23Tot", "core23Tot", "ntc23Tot", "L24Tot", "core24Tot", "ntc24Tot", "L34Tot", "core34Tot", "ntc34Tot")]

# Merge pr/po/tenOrMore totals by UID (N = 171)
totals <- merge(theta, tenOrMoreTotals, by = "uid")
levels <- merge(theta, tenOrMoreLevels, by = "uid")
combos <- merge(theta, tenOrMoreCombos, by = "uid")

tot <- totals[, 4:ncol(totals)] # Highest correlation with nonTutorialCoreTot -> 0.4227186  0.3471241  0.3519837 0.4313662
lev <- levels[, 4:ncol(levels)] # Highest correlation with Level 3, L3Tot -> 0.4711034  0.4450931  0.4273116 0.4330594 (only 7, 4, and 3 levels...)
com <- combos[, 4:ncol(combos)] #
# Pearson correlation
cor(tot,tot)[,c(1:5)] 
cor(lev,lev)[,c(1:5)] 
cor(com,com)[,c(1:5)] 

summary(tenOrMore)
hist(combos$L24Tot, col=5, breaks=20)
hist(combos$core24Tot, col=5, breaks=20)
hist(combos$ntc24Tot, col=5, breaks=20)



# Load in game response vectors from Seth's data cleaning
resp_val <- read.table("./Data/gameRespVec.csv", fill = TRUE, header = TRUE, sep = ",")
respTot_val <- resp_val[, c(1,ncol(resp_val))] # Number of correct 1st attempts per player
df_val <- merge(theta, respTot_val, by = "uid")
num_val <- df_val[4:8]
# Pearson correlation
cor(num_val,num_val) # pr/po data is positively correlated.
# 0.1735607, slight positive correlation Grand.Total with po_est

# Correlation between in-game performance and pr/po ability estimates 
gameperf <- tot$allTot
thetapo <- tot$po_est
diff <- combos$diff
core24 <- combos$core24Tot
cor(gameperf,thetapo) # allTot: 0.2706906 | core24: 0.5008602 | L24Tot: 0.5279933 (slight positive correlation)

# 1:1 Plot of Game Performance vs Post Ability Estimates
plot(gameperf, thetapo,
     pch = 16,
     xlab="Levels 1-4 Game Performance",
     ylab="Ability Post")
abline(lm(thetapo ~ gameperf), col="red", lwd=2)
abline(-4,2/3, col="blue", lwd=1, lty="dashed" )

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

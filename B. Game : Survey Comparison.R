# Merge game response vectors with pr/po data for correlation stats

# Library Imports
library(tidyverse)
library(reshape2)

# ===== Load in game answer submissions =====
ans <- read.table("./Data/submit_answer.csv", fill = TRUE, header = TRUE, sep = ",")
# Load in pr/po scores and ability estimates (thetas)
theta <- read.table("./Data/prpotheta.csv", fill = TRUE, header = TRUE, sep = ",") 
theta <- cbind(theta, diff=theta$po_est-theta$pr_est) # add column with difference between ability estimates (po - pr)

# Select 1st attempts only
ans <- ans[which(ans$attempt_count == 1), ] # 8524 rows
ans <- ans[ order(ans[,2], ans[,5]), ] # Order by userId and gameLevel
# Find duplicate uid/game level
ans <- ans[!duplicated(ans[c("userId","gameLevel")]), ] # Remove duplicates (8407 rows)
# Remove hand-feeding challenges T2.01 T1.01 T2.01.REPLACE (adult ram, baby ram, 3 baby ram)
ans <- setdiff(ans, ans[which(ans$gameLevel %in% c("T2.01", "T1.01", "T2.01-REPLACE")), ])

# ===== Create gameLevel lists ============
level_types <- ans[, c(5,8)]
level_types <- level_types[!duplicated(level_types$gameLevel), ] 
level_types <- level_types[ order(level_types[,1]), ]
# Look at levels across the entire game (totals_list)

all_levels <- c("T1.02a", "T1.02b", "T1.03", "T1.05", "T1.04", "T1.07a", "1.05b", "1.03a", "1.03c", "1.02b", "1.02c", "1.01a", "1.01b", "1.09a", "1.08", "1.07a", "1.07b", "1.05a", "1.05c", "1.04a", "1.06", "2.01c", "2.02a", "2.02c", "2.01a", "2.01b", "2.06b", "2.05a", "2.04b", "2.06a", "2.05b", "2.03b", "2.04a", "2.03a", "T3.01a", "3.01b", "3.01a", "3.01c", "3.03b", "3.03a", "3.03d", "T4.01a", "T4.01b", "T4.02", "T4.03a", "4.07b", "4.04a", "4.03a", "4.03b", "4.03c", "4.02a", "4.1", "4.16", "4.17", "4.15", "4.08", "4.11", "4.13") #58 levels
# Since review challenges are not seen by all players, subset into core challenges
core_levels <- c("T1.02a", "T1.03", "T1.05", "T1.04", "T1.07a", "1.05b", "1.09a", "1.08", "1.07a", "1.06", "2.01c", "2.06b", "2.05a", "2.04b", "2.03a", "T3.01a", "3.01b", "3.03b", "3.03d", "T4.01a", "T4.01b", "T4.02", "T4.03a", "4.07b", "4.1", "4.16", "4.17", "4.13") #28 levels
review_levels <- c("T1.02b", "1.03a", "1.03c", "1.02b", "1.02c", "1.01a", "1.01b", "1.07b", "1.05a", "1.05c", "1.04a", "2.02a", "2.02c", "2.01a", "2.01b", "2.05b", "2.06a", "2.03b", "2.04a", "3.01a", "3.01c", "3.03a", "4.04a", "4.03a", "4.03b", "4.03c", "4.02a", "4.15", "4.08", "4.11") #30 levels
# Check tutorial vs non-tutorial levels
non_tutorial_core <- c("1.05b", "1.09a", "1.08", "1.07a", "1.06", "2.01c", "2.06b", "2.05a", "2.04b", "2.03a", "3.01b", "3.03b", "3.03d", "4.07b", "4.1", "4.16", "4.17", "4.13") #18 levels
tutorial_core <- c("T1.02a", "T1.03", "T1.05", "T1.04", "T1.07a", "T3.01a", "T4.01a", "T4.01b", "T4.02", "T4.03a") #10 levels
totals_list <- list(all=all_levels, core=core_levels, review=review_levels, ntc=non_tutorial_core, tc=tutorial_core)
# Since game is teaching along the way, subset game performance into levels (levels_list) {L1, L2, L3, L4} {core, core1, core2, core3, core4} {review1, review2, review3, review4} {ntc1, ntc2, ntc3, ntc4}
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
levels_list <- list(L1=c(core1_levels, review1_levels), core1=core1_levels, review1=review1_levels, ntc1=ntc1_levels, L2=c(core2_levels, review2_levels), core2=core2_levels, review2=review2_levels, ntc2=ntc2_levels, L3=c(core3_levels, review3_levels), core3=core3_levels, review3=review3_levels, ntc3=ntc3_levels, L4=c(core4_levels, review4_levels), core4=core4_levels, review4=review4_levels, ntc4=ntc4_levels)
# Check level combinations after level 1 (combos_list) {2-3, 2-4, 3-4} 
combos_list <- list(L23=c("L2","L3"), core23=c("core2", "core3"), review23=c("review2", "review3"), ntc23=c("ntc2", "ntc3"), L24=c("L2","L3", "L4"), core24=c("core2", "core3", "core4"), review24=c("review2", "review3", "review4"), ntc24=c("ntc2", "ntc3", "ntc4"), L34=c("L3", "L4"), core34=c("core3", "core4"), review34=c("review3", "review4"), ntc34=c("ntc3", "ntc4"))

# ===== Exploring gameLevels =====

ans$gameLevel <- as.character(ans$gameLevel)
ans$success <- as.factor(ans$success)
g <- ggplot(ans[which(ans$gameLevel %in% non_tutorial_core), c("gameLevel", "success")])
g + geom_bar((aes(x=factor(gameLevel, levels=non_tutorial_core), fill=success))) +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) + 
  xlab("Non-Tutorial Core Levels") +
  ggtitle("All Player (N = 697) Prima Beta Performance Across Non-Tutorial Core Levels")

# ===== Explore gameLevelSequences =====
# View gameLevel sequences
ans[order(ans[,2], ans[,1]), c("userId", "clientTimeStamp", "gameLevel", "success")]
# Create df of sequences for each userId
seq <- ans[order(ans[,2], ans[,1]), c("userId", "clientTimeStamp", "gameLevel", "success")]
seq$gameLevel <- sapply(ans$gameLevel, function(x) match(x, all_levels))
seq <- seq[,c(1,3)] # Only uid and gameLevel
seq %>% 
  mutate(level = case_when(
    lag(.$userId) != .$userId ~"Start",
    lead(.$userId) != .$userId ~"End",
    TRUE ~ "other"
  )
  ) -> seq
seq$level[1] <- "Start"
seq$level[7325] <- "End"
# Sequence lengths
len <- aggregate(gameLevel~userId,seq,length)
names(len)[2] <- "length"
seq <- merge(seq, len, by="userId")
seq$level <- as.factor(seq$level)
seq10 <- seq[which(seq$length > 10),]
s <- ggplot(seq10) #"dodgerblue", "goldenrod","darkorchid","chocolate"
s + geom_point((aes(x=factor(all_levels[gameLevel], levels=all_levels), y=userId, color=level))) +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) + 
  xlab("Game Challenge") +
  ggtitle("All Player (N = 697) Prima Beta Level Sequences") +
  scale_color_manual(values=c("black","gray","red")) 

# Distribution of sequence lengths

boxplot(len$gameLevel,data=len, main="Sequence Lengths", 
        ylab="Length of Game Challenge Sequence")
hist(len$gameLevel)

# ===== Pivot answer submissions to make response vectors =====
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

# ===== Exploring response vectors =====

# ===== Feature Engineering on All challenges completed (N = 697) =====
# Find number complete (Comp), total correct (Tot), and percent correct (Per) across game data response vectors 
for (i in 1:length(totals_list)) {
  complete <- paste0(names(totals_list)[[i]], "Comp")
  resp[,complete] <- sapply(1:nrow(resp), function(x) sum(!is.na(resp[x,totals_list[[i]]]))) # Count number of challenges completed (0 or 1)
  correct <- paste0(names(totals_list)[[i]], "Tot")
  percent <- paste0(names(totals_list)[[i]], "Per")
  resp %>% select(which(names(resp) %in% totals_list[[i]])) %>% rowSums(na.rm=TRUE) -> resp[,correct]
  resp[,percent] <- resp[,correct] / resp[,complete]
}

for (i in 1:length(levels_list)) {
  complete <- paste0(names(levels_list)[[i]], "Comp")
  resp[,complete] <- sapply(1:nrow(resp), function(x) sum(!is.na(resp[x,levels_list[[i]]]))) # Count number of challenges completed (0 or 1)
  correct <- paste0(names(levels_list)[[i]], "Tot")
  percent <- paste0(names(levels_list)[[i]], "Per")
  resp %>% select(which(names(resp) %in% levels_list[[i]])) %>% rowSums(na.rm=TRUE) -> resp[,correct]
  resp[,percent] <- resp[,correct] / resp[,complete]
}

for (i in 1:length(combos_list)) {
  complete <- paste0(names(combos_list)[[i]], "Comp")
  resp %>% select(which(names(resp) %in% sapply(combos_list[[i]], function(x) paste0(x, "Comp")))) %>% rowSums(na.rm=TRUE) -> resp[,complete]
  correct <- paste0(names(combos_list)[[i]], "Tot")
  percent <- paste0(names(combos_list)[[i]], "Per")
  resp %>% select(which(names(resp) %in% sapply(combos_list[[i]], function(x) paste0(x, "Tot")))) %>% rowSums(na.rm=TRUE) -> resp[,correct]
  resp[,percent] <- resp[,correct] / resp[,complete]
}

# Substitute NA for NaN
#resp[, sapply(names(totals_list), function(x) paste0(x, "Per"))][apply(resp[, sapply(names(totals_list), function(x) paste0(x, "Per"))], c(1,2), is.nan)] <- NA
#resp[, sapply(names(levels_list), function(x) paste0(x, "Per"))][apply(resp[, sapply(names(levels_list), function(x) paste0(x, "Per"))], c(1,2), is.nan)] <- NA
#resp[, sapply(names(combos_list), function(x) paste0(x, "Per"))][apply(resp[, sapply(names(combos_list), function(x) paste0(x, "Per"))], c(1,2), is.nan)] <- NA

# ===== Compare correlations (All) ====
respTotals <- resp[, c("uid", sapply(names(totals_list), function(x) paste0(x, "Per")))]
respLevels <- resp[, c("uid", sapply(names(levels_list), function(x) paste0(x, "Per")))]
respCombos <- resp[, c("uid", sapply(names(combos_list), function(x) paste0(x, "Per")))]

# Merge pr/po/resp totals by UID (N = 266)
totals <- merge(theta, respTotals, by = "uid")
levels <- merge(theta, respLevels, by = "uid")
combos <- merge(theta, respCombos, by = "uid")

tot <- totals[, 4:ncol(totals)] # Largest positive correlation with nonTutorial levels -> nonTutorialCoreTot 0.27711696 0.2877823 0.28520689 0.26184614
lev <- levels[, 4:ncol(levels)] # first review levels negatively correlate with survey performance review1Tot -0.30359333 -0.28300671 -0.28809288
com <- combos[, 4:ncol(combos)]
# Pearson correlation
cor(tot,tot, use = "pairwise.complete.obs")[,c(1:5)] # ntcPer -> 0.306401966  0.313457027  0.307394983 0.29564089
cor(lev,lev, use = "pairwise.complete.obs")[,c(1:5)] # L2Per -> 0.42543938  0.364881067  0.35651727  0.41870486, ntc1Per -> 0.41416636  0.425645808  0.44177937  0.41298730
cor(com,com, use = "pairwise.complete.obs")[,c(1:5)] 
# pr/po data is positively correlated. 
# Game data total response vector percentage (allPer) are weakly correlated to survey data (0.27 pr, 0.23 po)
# Non-tutorial Core levels 1 percentage (ntc1Per) have a positive correlation (0.44 pr  0.41 po)
# Survey Slopes (diff) have a negative correlation with pre and a positive correlation with post (-0.266 pr, 0.389 po)

model <- lm(levels$po_est ~ levels$ntc1Per)
anova(model)
summary(model)
plot(model)

# ===== Feature Engineering on 10 or more challenges completed (N = 373)  =====
resp$completed <- sapply(1:nrow(resp), function(x) sum(!is.na(resp[x, all_levels])))
tenOrMore <- resp[which(resp$completed >= 10),]

# Percent Correct feature for full-game challenge subsets
for (i in 1:length(totals_list)) {
  complete <- paste0(names(totals_list)[[i]], "Comp")
  tenOrMore[,complete] <- sapply(1:nrow(tenOrMore), function(x) sum(!is.na(tenOrMore[x,totals_list[[i]]]))) # Count number of challenges completed (0 or 1)
  correct <- paste0(names(totals_list)[[i]], "Tot")
  percent <- paste0(names(totals_list)[[i]], "Per")
  tenOrMore %>% select(which(names(tenOrMore) %in% totals_list[[i]])) %>% rowSums(na.rm=TRUE) -> tenOrMore[,correct]
  tenOrMore[,percent] <- tenOrMore[,correct] / tenOrMore[,complete]
}

# Percent Correct feature for single level challenge subsets
for (i in 1:length(levels_list)) {
  complete <- paste0(names(levels_list)[[i]], "Comp")
  tenOrMore[,complete] <- sapply(1:nrow(tenOrMore), function(x) sum(!is.na(tenOrMore[x,levels_list[[i]]]))) # Count number of challenges completed (0 or 1)
  correct <- paste0(names(levels_list)[[i]], "Tot")
  percent <- paste0(names(levels_list)[[i]], "Per")
  tenOrMore %>% select(which(names(tenOrMore) %in% levels_list[[i]])) %>% rowSums(na.rm=TRUE) -> tenOrMore[,correct]
  tenOrMore[,percent] <- tenOrMore[,correct] / tenOrMore[,complete]
}

# Percent Correct feature for later level combination challenge subsets
for (i in 1:length(combos_list)) {
  complete <- paste0(names(combos_list)[[i]], "Comp")
  tenOrMore %>% select(which(names(tenOrMore) %in% sapply(combos_list[[i]], function(x) paste0(x, "Comp")))) %>% rowSums(na.rm=TRUE) -> tenOrMore[,complete]
  correct <- paste0(names(combos_list)[[i]], "Tot")
  percent <- paste0(names(combos_list)[[i]], "Per")
  tenOrMore %>% select(which(names(tenOrMore) %in% sapply(combos_list[[i]], function(x) paste0(x, "Tot")))) %>% rowSums(na.rm=TRUE) -> tenOrMore[,correct]
  tenOrMore[,percent] <- tenOrMore[,correct] / tenOrMore[,complete]
}

# ===== Compare correlations (10 or more) =====
tenOrMoreTotals <- tenOrMore[, c("uid", sapply(names(totals_list), function(x) paste0(x, "Per")))]
tenOrMoreLevels <- tenOrMore[, c("uid", sapply(names(levels_list), function(x) paste0(x, "Per")))]
tenOrMoreCombos <- tenOrMore[, c("uid", sapply(names(combos_list), function(x) paste0(x, "Per")))]

# Merge pr/po/tenOrMore totals by UID (N = 171)
totals <- merge(theta, tenOrMoreTotals, by = "uid")
levels <- merge(theta, tenOrMoreLevels, by = "uid")
combos <- merge(theta, tenOrMoreCombos, by = "uid")

tot <- totals[, 4:ncol(totals)] # Highest correlation with ntcPer -> 0.296765128  0.240001000  0.24317275 0.31724729
lev <- levels[, 4:ncol(levels)] # Highest correlation with Level 1, L1Per -> 0.424258605  0.40791854  0.43322743  0.46121995 
com <- combos[, 4:ncol(combos)] # Highest correlation with nontutorial lvls 2-3, ntc23Per -> 0.20866994  0.20191871  0.19075794  0.21409736 
# Pearson correlation
cor(tot,tot, use = "pairwise.complete.obs")[,c(1:5)] 
cor(lev,lev, use = "pairwise.complete.obs")[,c(1:5)] 
cor(com,com, use = "pairwise.complete.obs")[,c(1:5)] 

summary(tenOrMore)
hist(combos$L24Per, col=5, breaks=20)
hist(levels$core24Per, col=5, breaks=20)
hist(totals$ntcPer, col=5, breaks=20)



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

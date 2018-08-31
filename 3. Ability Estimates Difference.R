# Ability estimates from pre to post
# Was there a significant change in ability estimates from pre to post?
# It looks that there was a significant difference, but the effect was negligible

# ================== Merge pr/po scores to ability estimates ===========================
# Load data files
abilitypr <- read.table("./Data/prAbility.csv", header=TRUE, sep=",", as.is = TRUE)
abilitypo <- read.table("./Data/poAbility.csv", header=TRUE, sep=",", as.is = TRUE)
merge <- read.table("./Data/mergeSurvey.csv", header=TRUE, sep=",", as.is = TRUE)

# Select vars of interest
df <- merge[,c("uid", "tchr", "g", "pr_raw", "post.rawscr")]
names(df)[names(df) == "post.rawscr"] <- "po_raw"
summary(df)
pr <- abilitypr[,c("pr_raw","pr_est")]; po <- abilitypo[,c("po_raw","po_est")]

# Merge estimates with pr/po raw scores
df <- merge(df, pr, by="pr_raw")
df <- merge(df, po, by="po_raw")
df <- df[,c(3,4,5,1,2,6,7)]
# Write merge to file
library(readr) # For CSV file I/O
write_csv(df, "./Data/prpotheta.csv", na = "NA", append = FALSE, col_names = TRUE)

# ================== Test Difference ===========================
df <- cbind(df, diff=df$po_est-df$pr_est) # add column with difference between ability estimates (po - pr)
summary(df)
hist(df$pr_est, col=5, breaks=20)
hist(df$po_est, col=5, breaks=20)
hist(df$diff, col=5, breaks=20)

# Plot of Differences
plot(df$diff,
     pch = 16,
     ylab="Difference (Post Estimate – Pre Estimate)")
abline(0,0, col="blue", lwd=2)

# paired t-test
t.test(df$pr_est, df$po_est, paired=TRUE)  # significant difference in means

# 1:1 Plot of Ability Estimates
plot(df$pr_est, df$po_est,
     pch = 16,
     xlab="Pre Estimates",
     ylab="Post Estimates")
abline(0,1, col="blue", lwd=2)

# effsize
library(effsize)
## compute Cohen's d
cohen.d(df$po_est, df$pr_est) # negligible effect
## compute Hedges' g
cohen.d(df$po_est, df$pr_est, hedges.correction=TRUE) # negligible effect


# Notice the shape of these histograms
# There is a large incidence of po_raw scores of 0
# This may include many students who didn't bother to do the survey
hist(df$pr_raw, col=5, breaks=20)
hist(df$po_raw, col=5, breaks=20)

raw_diff <- df$po_raw - df$pr_raw
hist(raw_diff, col=5, breaks=20)

t.test(df$pr_raw, df$po_raw, paired=TRUE)
cohen.d(df$po_raw, df$pr_raw)

zeropr <- df[which(df$pr_raw == 0), ]
zeropo <- df[which(df$po_raw == 0), ]

# Histograms of Zero scores
hist(zeropr$po_raw, col=5, breaks=20)
hist(zeropo$pr_raw, col=5, breaks=20) # only 2 students scored over 5 in pr and then took a 0 in po

# Look at subsets of scores (1-8), (9-16), (17-24) and how they performed pr->po
lopr <- df[which(df$pr_raw < 9), ]
midpr <- df[which(df$pr_raw > 8 | df$pr_raw < 17), ]
hipr <- df[which(df$pr_raw > 16), ]

hist(lopr$po_raw, col=5, breaks=20)
hist(midpr$po_raw, col=5, breaks=20)
hist(hipr$po_raw, col=5, breaks=20)

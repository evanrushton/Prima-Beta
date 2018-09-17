# ================== Test Difference ===========================
# Ability estimates from pre to post (survey data)
# Was there a significant change in ability estimates from pre to post?
# It looks that there was a significant difference, but the effect was negligible

# Load data files
df <- read.table("./Data/prpotheta.csv", header=TRUE, sep=",", as.is = TRUE)
df <- cbind(df, diff=df$po_est-df$pr_est) # add column with difference between ability estimates (po - pr)
summary(df)
hist(df$pr_est, col=5, breaks=20)
hist(df$po_est, col=5, breaks=20)
hist(df$diff, col=5, breaks=20)


# Plot of Differences
plot(x=df$pr_est,
     y=df$po_est, 
     pch = 16,
     xlab="Pre Estimate",
     ylab="Post Estimate")
abline(0,1, col="blue", lwd=2)

# paired t-test
t.test(df$po_est, df$pr_est, paired=TRUE)  # significant difference in means

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

cor(df[4:8], use="complete.obs", method="pearson") 
cov(df[4:8], use="complete.obs", method="pearson") 

# Correlations with significance levels
library(Hmisc)
rcorr(as.matrix(df[4:8]), type="pearson") # type can be pearson or spearman

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

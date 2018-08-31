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

# Write merged df to file
library(readr) # For CSV file I/O
write_csv(df, "./Data/prpotheta.csv", na = "NA", append = FALSE, col_names = TRUE)
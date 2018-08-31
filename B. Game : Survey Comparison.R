# Merge Game response vectors with pr/po data for correlations

# Library Imports
library(readr) # CSV file I/O
library(dplyr) # Data wrangling

# Load in game response vectors
resp <- read.table("./Data/gameRespVec.csv", fill = TRUE, header = TRUE, sep = ",")
respTot <- resp[, c(1,ncol(resp))]

# Load in pr/po scores and ability estimates (thetas)
scrpr <- read.table("./Data/prsurvey.csv", fill = TRUE, header = TRUE, sep = ",") 
scrpo <- read.table("./Data/posurvey.csv", fill = TRUE, header = TRUE, sep = ",")
thetapr <- read.table("./Data/prAbility.csv", fill = TRUE, header = TRUE, sep = ",")
thetapo <- read.table("./Data/poAbility.csv", fill = TRUE, header = TRUE, sep = ",")

# Merge pr/po/resp totals by UID
scrTotpr <- scrpr[1:4]
scrTotpo <- scrpo[, c(1,3)]

# Correlation between in-game performance and pr/po scores and ability estimates


# Since game is teaching along the way, subset game performance into levels {L1, L2, L3, L4}
# LCA of Linking Items

# Read in the cleaned pre-test and post-test data (see 0. Clean Survey Data.R) 
pr <- read.table("./Data/prresponses.csv", header=TRUE, sep=",", as.is = TRUE) # [1] 594  29
po <- read.table("./Data/poresponses.csv", header=TRUE, sep=",", as.is = TRUE) # [1] 468  27

# Create vectors to categorize responses to linking items (3, 12, 16, 21, 34, 40)
linking_items <- c("X.3.MD.UR.pr..r", "X.12.MD.NUR_L.r", "X.16.MS.NUR_L.r", "X.21.ND.NUR_L.r", "X.34.MT.UR_L.r", " X.40.MT.NUR_L.r")
additive_responses <- c(11, 15, 39, 20, 48, 75)
magic_doubling <- c(16, 14, 8, 36)
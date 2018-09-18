# Clean original Surveygizmo .csv dumps for prea/preb forms and posta/postb forms

# Read in the raw pre-test and post-test data  
prA <- read.delim("./Data/prA.csv", header=TRUE, sep=",", as.is = TRUE) # 436  35
prB <- read.delim("./Data/prB.csv", header=TRUE, sep=",", as.is = TRUE) # 422  35
poA <- read.delim("./Data/poA.csv", header=TRUE, sep=",", as.is = TRUE) # 302  35
poB <- read.delim("./Data/poB.csv", header=TRUE, sep=",", as.is = TRUE) # 328  35

my_data <- list(prA, prB, poA, poB)

# Remove questions from the item name
pattern <- '(X.\\w+.\\w+.\\w+[._]\\w+)(\\S+)' # \\1 <- X.21.ND.NUR_L  \\2 <- .....6.trains.carry.a.total.of.18.cars..How.many.cars.will.8.trains.carry.... 
level <- "\\1"
# question <- "\\2"
# Set questions as captured question group
# questions <- lapply(names(prA), function(y) gsub(pattern, question, y)) # (LEVEL)QUESTION -> QUESTION

# Remove question from level name
FixNames <- function(df, whichVar, i) {
  
}
my_data <- sapply(names(my_data), function(y) gsub(pattern, level, y, perl=TRUE)) # (LEVEL)QUESTION -> LEVEL
  # Change variable names
  names(df)[c(6,7,8)] <- c("First", "Last", "uid")
}



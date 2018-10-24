# Helper Functions

# ===== Cleaning ======

# Clean the gaem data rows with actionName = "submit_answer"
clean_submit_answer_data <- function(df) {
  ans <- df[ which(df$actionName == "submit_answer"), ] 
  
  #### Text to Columns ####
  split <- str_split(ans$actionData, ',')
  ans$challenge_type <- sapply(split, '[[', 1)
  ans$problem_type <- sapply(split, '[[', 2)
  ans$success <- sapply(split, '[[', 3)
  ans$attempt_count <- sapply(split, '[[', 4)
  ans$latency <- sapply(split, '[[', 5) 
  # Remove actionData
  ans <- ans[,!(names(ans) %in% c("actionData"))]  
  
  #### Clean ans actionData - use column names rather than references ####
  pattern <- '^.*:(\\w*)' #  HEADER:VALUE, \\1 <- VALUE
  replacement <- "\\1"
  cols <- c("challenge_type", "problem_type", "success", "attempt_count", "latency")
  ans[,cols] <- sapply(ans[,cols], function(y) gsub(pattern, replacement, y, perl=T)) # HEADER:VALUE -> VALUE
  ans <- as.data.frame(ans)
  # Remove } and \"
  ans[,"latency"] <- sapply(ans[,"latency"], function(y) gsub("}", "", y))
  ans <- as.data.frame(ans)
  ans[,c("challenge_type", "problem_type")] <- sapply(ans[,c("challenge_type", "problem_type")], function(y) gsub("[[:punct:]]", "", y))
  ans <- as.data.frame(ans)
  
  return(ans)
}

# 
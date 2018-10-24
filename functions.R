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
  
  #### Clean ans actionData ####
  pattern <- '^.*:(\\w*)' #  HEADER:VALUE, \\1 <- VALUE
  replacement <- "\\1"
  ans[,5:9] <- sapply(ans[,5:9], function(y) gsub(pattern, replacement, y, perl=T)) # HEADER:VALUE -> VALUE
  ans <- as.data.frame(ans)
  # Remove } and \"
  ans[,9] <- sapply(ans[,9], function(y) gsub("}", "", y))
  ans <- as.data.frame(ans)
  ans[,5:6] <- sapply(ans[,5:6], function(y) gsub("[[:punct:]]", "", y))
  ans <- as.data.frame(ans)
  
  return(ans)
}

# 
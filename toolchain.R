source("svninfo.R")

serializeResultAndModel <-
  function(model_path, model, result_path, result)
  {
    result$Survived <- as.character(result$Survived)
    result$Survived[result$Survived == "Died"] <- 0
    result$Survived[result$Survived == "Survived"] <- 1
    result$Survived <- as.integer(result$Survived)
    write.csv(paste0(result_path, "result_rev", as.character(GetRevision()), ".csv"),
              x = result,
              row.names = FALSE)
    saveRDS(model, paste0(model_path, "model_rev", as.character(GetRevision()), ".rdata"))
  }

serializeFactor <- function(factor_path, factor)
{
  saveRDS(model, paste0(factor_path, "factor_rev", as.character(GetRevision()), ".rdata"))
}

loadData <- function(path, file, colClasses)
{
  return (read.csv(paste0(path, file), colClasses = colClasses, stringsAsFactors = F))
}

validate <- function(model, data, score)
{
  pred <- predict(model, newdata = data)
  #pred <- apply(pred, 2, round)[, 2]
  result <- cbind(data[, c(1, 2)], pred)
  nr <- nrow(result[result$Survived==result$pred, ])
  score <<- (nrow(result[result$Survived==result$pred, ]) / nrow(data))
  return (result)
}
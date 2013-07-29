#' Collects all Epochs in an Experiment or EpochGroup
#' 
#' Recursively collects all Epochs in an Experiment or EpochGroup into a
#' Vector. Epoch order is not guaranteed. You may want to sort the resulting
#' vector by Epoch \code{startTime}.
#' 
#' @param container Experiment or EpochGroup
#' @return Vector of Epochs
#' @export
CollectEpochs <- function(container) {
  # Collect Epochs, and then Epochs from all EpochGroups
  epochs <- as.list(container$getEpochs());
  
  for(g in as.list(container$getEpochGroups())) {
    epochs <- c(epochs, CollectEpochs(g));
  }
  
  
  return(epochs);
}

#' Collect tabular data from Measurements of collected Epochs.
#' 
#' Creates a single DataFrame object that concatenates rows from 
#' all Measurements with content type "text/csv" or "application/csv"
#' in the provided Epochs.
#' 
#' @param epochs list of Epochs
#' @return DataFrame from CSV measurements in Epochs
#' @export
CollectMeasurements <- function(epochs) {
  result <- data.frame()
  source.ids <- c()
  source.labels <- c()
  
  for(e in epochs) {
    date <- e$getStart()
    
    plot.names <- as.list(e$getInputSources()$keySet())
    epoch.df <- data.frame()
    
    for(m in as.list(e$getMeasurements())) {
      content.type <- m$getDataContentType()
      if(content.type == "text/csv" || content.type == "application/csv") {
        measurement.df <- read.csv(m$getLocalDataPath()$get())
        
        result <- rbind.fill(result, measurement.df)
      }
    }
  }
  
  if(!is.null(source.label.colname) && !(source.label.colname %in% colnames(result))) {
    result[source.label.colname] = as.factor(source.labels)
  }
  
  if(!is.null(source.id.colname) && !(source.id.colname %in% colnames(result))) {
    result[source.id.colname] = as.factor(source.ids)
  }
  
  
  return(result)
}

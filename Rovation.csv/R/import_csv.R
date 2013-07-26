library(plyr)

#' Inserts Ovation Measurements for each row in a DataFrame
#' 
#' @param context
#' @param csv.path
#' @param start.time
#' @param end.time
#' @param protocol.uri
#' @param protocol.parameters
#' @param devices
#' @param device.parameters
#' @param container.uri
#' @param source.label.column
#' @param source.id.column
#' @return newly inserted Epoch
#' @export
ImportCSV <- function(context, 
                      csv.path, 
                      start.time, 
                      end.time, 
                      protocol.uri, 
                      protocol.parameters, 
                      devices, 
                      device.parameters, 
                      container.uri,
                      source.label.column = 1,
                      source.id.column = 2) {
  
  df <- read.csv(csv.path)
  
  container <- context$getObjectWithURI(container.uri)
  protocol <- context$getObjectWithURI(protocol.uri)
  
  # Insert Epoch
  cat("Inserting Epoch...\n")
  epoch <- container$insertEpoch(start.time,
                                 end.time,
                                 protocol,
                                 List2Map(protocol.parameters),
                                 List2Map(device.parameters))
  
  # Insert a measurement for each row
  measurements <- d_ply(df, c(df[[source.label.column]], df[[source.id.column]]), 
                        function(r) {
                          source.label <- unique(as.character(r[[source.label.column]]))
                          source.id <- unique(as.character(r[[source.id.column]]))
                          source.name <- sprintf("%s-%s", source.label, source.id)
                          
                          cat(sprintf("  %s\n", source.name))
                          
                          sources <- as.list(context$getSources(source.label, source.id))
                          
                          if(length(sources) == 0) {
                            source <- context$insertSource(source.label, source.id)
                          } else if (length(sources) == 1) {
                            source <- sources[[1]]
                          } else {
                            simpleError(sprintf("Multiple Sources with label %s and ID %s exist in the database"))
                          }
                          
                          epoch$addInputSource(source.name, source)
                          
                          temp.file <- tempfile(fileext=".csv")
                          write.csv(r, file=temp.file, row.names=FALSE)
                          epoch$insertMeasurement(source.name,
                                                  Vector2Set(c(source.name)),
                                                  Vector2Set(c()),
                                                  NewUrl(temp.file),
                                                  "text/csv")
                        })
  
  return(epoch)
}
library(plyr)

ImportCSV <- function(context, 
                      csv.path, 
                      start.time, 
                      end.time, 
                      protocol, 
                      protocol.parameters, 
                      devices, 
                      device.parameters, 
                      container,
                      source.id.column = 1) {
  
  df <- read.csv(csv.path)
  
  
}
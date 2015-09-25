corr <- function(directory, threshold = 0){
  monitors <- 1:332
  completeObservations <- complete(directory,monitors)
  functionalObservations <- c()
  corrList <- c()
  monitorFrames <- c()
  
  for(monitor in monitors){
    currentCompleteMonitor <- completeObservations[monitor,]
    if(currentCompleteMonitor$nobs > threshold){
      print(paste("Monitor #",monitor,"Meets the threshold",sep=" "))
      functionalObservations <- c(functionalObservations,monitor)
    }
  }
  
  print(paste("Amount to corr",length(functionalObservations),sep = " "))
  print(paste("files to corr",functionalObservations,sep = " "))
  monitorFrames<- readFiles(directory,functionalObservations)
  for(currentObs in functionalObservations){
    currentFrame <- monitorFrames[[currentObs]]
    corrList <- c(corrList, cor(currentFrame$sulfate,
                      currentFrame$nitrate,
                      use ="pairwise.complete.obs"))
  }
  corrList
}
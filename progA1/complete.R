complete <- function(directory, files){
  data.files <- readFiles(directory,files)
  finalObs<- data.frame(id= numeric(0), nobs=numeric(0))
    for(file in files){
      finalObs[nrow(finalObs)+1, ] <- c(file,countComplete(data.files[file]))
    }
  finalObs
}

countComplete <- function(toAnalyze){
  #There is no need for cell-by-cell comparison of NA
  completed <- complete.cases(toAnalyze[[1]])
  completeAmount <-sum(completed)
  print(paste("Amount of complete observations:",completeAmount,sep = ))
  completeAmount
}

readFiles <- function(folder, fileName){
  csvs <- c()
  for(i in fileName){
    finalFile <- getFileName(folder,i)
    print(paste("reading File",finalFile,sep=" "))
    csvs[[i]] <- read.csv(finalFile,header = TRUE)
  }
  print(paste("Files loaded:",length(csvs),sep=" "))
  csvs
}

getFileName <- function(folder,name){
  finalFile <- paste(paste(getwd(),folder,sep="/"),
                     paste(sprintf("%03d",name),"csv",sep="."), sep="/")
  finalFile
  
}
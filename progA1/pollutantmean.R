pollutantmean <- function(directory, pollutant, id = 1:332) {
  data.files <- readFiles(directory,id)
  print(paste("Selected pollutant",pollutant, sep=" "))
  mean <- calculateMean(data.files,pollutant,id)
  mean
}
#Actual calculation of clean vectors
calculateMean <- function(loadedFiles,pollutant,monitors){
  values <- c()
  for(file in monitors){
    currentFile <- loadedFiles[[file]]
    #it has to be one large vector in order to get the numbers right
    values <- c(values,currentFile[,pollutant])
  }
  result <- mean(values,na.rm = TRUE)
  print(paste("Mean for pollutant",pollutant,":",result,sep=" "))
  result
}
#Read and load files
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

#create the string for the file to read
getFileName <- function(folder,name){
  finalFile <- paste(paste(getwd(),folder,sep="/"),
                     paste(sprintf("%03d",name),"csv",sep="."), sep="/")
  finalFile
  
}
pollutantmean <- function (directory, pollutant, id=1:332) {
    cvsFiles <- list.files(path = directory, full.names = TRUE)
    selectedData <- data.frame()
    for (i in id) {
        selectedData <- rbind(selectedData, read.csv(cvsFiles[i]))
    }
    mean (selectedData[,pollutant], na.rm=TRUE)
}
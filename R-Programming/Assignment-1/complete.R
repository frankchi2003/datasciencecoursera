complete <- function (directory, id=1:332) {
    cvsFiles <- list.files(path = directory, full.names = TRUE)
    rc <- data.frame()
    for (i in id) {
        dataset <- read.csv(cvsFiles[i], header=TRUE)
        nobs <- sum(complete.cases(dataset))
        rc <- rbind(rc, data.frame(id=i,nobs))
    }
    rc
}

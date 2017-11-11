corr <- function (directory, threshold=0) {
    cvsFiles <- list.files(path = directory, full.names = TRUE)
    rc <- complete(directory)
    rcIds <- rc[rc$nobs>threshold,1]
    corrs <- rep(NA,length(rcIds))
    for (i in rcIds) {
        dataset <- read.csv(cvsFiles[i])
        compset <- complete.cases(dataset)
        x <- dataset[compset, 2]
        y <- dataset[compset, 3]
        corrs[i] <- cor(x, y)
    }
    corrs[complete.cases(corrs)]
}
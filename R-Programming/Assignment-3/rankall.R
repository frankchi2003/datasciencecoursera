library(stringr)

## function: 
##   rankall - Ranking hospitals in all states
## parameters:
##   outcome - an outcome name
##   num - the ranking of a hospital in that state for that outcome,
##         it can take values "best", "worst", or an integer indicating
##         the ranking.
## return:
##   returns a character vector with the name of the hospital that has 
##   the ranking specified by the num argument.
## note: The outcomes can be one of "heart attack", "heart failure", or "pneumonia".
##


rankall <- function(outcome, num = "best") {

    ## Initialization
    outcome_names <- c("heart attack", "heart failure", "pneumonia")
    outcome_cols  <- c(11 ,17 ,23)
    outcome_file  <- "outcome-of-care-measures.csv"
    arg1 <- tolower(str_replace(gsub("\\s+", " ", str_trim(outcome)), "B", "b"))
    arg1 <- paste("^", arg1, "$", sep="")
    
    ## Read outcome data
    oocm_data <- read.csv(outcome_file, colClasses = "character")
    allStates <- sort(unique(oocm_data[,7]))

    ## Check that outcome and num are valid
    outcom_col <- grep(arg1, outcome_names);
    if (length(outcom_col) == 0) {
        stop(paste("invalid outcome", outcome, sep=":"))
    }
    outcom_col <- outcome_cols[outcom_col]
    
    if (! (num == "best" || num == "worst" || is.numeric(num))) {
        stop(paste("invalid num", num, sep=":"))
    }

    ## For each state, find the hospital of the given rank
    ## query result set (col 1 = outcome, col 2 = hospital name, col 3 = State)
    rs <- oocm_data[ ,c(outcom_col, 2, 7)]
    colnames(rs) <- c("outcome", "hospital", "state")

    ## convert outcome to numeric
    suppressWarnings( rs$outcome<-as.numeric(rs$outcome) )
    rs<-subset(rs,!is.na(rs$outcome))
    
    ## order the result by State, outcome, hospital name
    rs <- rs[order(rs$state,rs$outcome,rs$hospital),]
    result <- data.frame()
    for (st in allStates) {
        rc  <- subset( rs, rs$state == st )
        r   <-  if (nrow(rc) == 0) 100
                else if (num == "best") 1
                else if (num == "worst") nrow(rc)
                else num
        df <- data.frame(row.names=st, hospital=rc[r,"hospital"],state=st)
        result <- rbind(result, df)
    }

    ## Return a data frame with the hospital names and the (abbreviated) state name
    return(result)
    
    
}

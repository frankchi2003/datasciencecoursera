library(stringr)

## function: 
##   best - Finding the best hospital in a state
## parameters:
##   state - the 2-character abbreviated name of a state 
##   outcome - an outcome name
## return:
##   a character vector with the name of the hospital that has the best 
##   (i.e. lowest) 30-day mortality for the specified outcome in that state.
## note: The outcomes can be one of "heart attack", "heart failure", or "pneumonia".
##
best <- function (state, outcome) {
    
    ## Initialization
    outcome_names <- c("heart attack", "heart failure", "pneumonia")
    outcome_cols  <- c(11 ,17 ,23)
    outcome_file  <- "outcome-of-care-measures.csv"
    arg1 <- toupper(state)
    arg2 <- tolower(str_replace(gsub("\\s+", " ", str_trim(outcome)), "B", "b"))
    arg2 <- paste("^", arg2, "$", sep="")
    
    ## Read outcom data
    oocm_data <- read.csv(outcome_file, colClasses = "character")
    allowStates <- toupper(unique(oocm_data[,7]))

    ## Check that state and outcome are valid
    if ( !(arg1 %in% allowStates) ) {
        stop(paste("invalid state", state, sep=":"))
    }
    
    outcome_loc <- grep(arg2, outcome_names);
    if (length(outcome_loc) == 0) {
        stop(paste("invalid outcome", outcome, sep=":"))
    }
    outcome_loc <- outcome_cols[outcome_loc]
    
    ## Return hospital name in that state with lowest 30-day death rate
    ## subset of outcome data w/ specific state (column 7 = State)
    sub_oocm_data <- subset( oocm_data, toupper(oocm_data[,7]) == arg1 )
    
    ## query result set (col 1 = outcome, col 2 = hospital name)
    rs <- sub_oocm_data[ ,c(outcome_loc, 2)]

    ## convert outcome to numeric
    suppressWarnings( rs[,1]<-as.numeric(rs[,1]) )
    rs<-subset(rs,!is.na(rs[,1]))

    ## order the result by outcome
    rc <- rs[order(rs[,1],rs[,2]),2]
    
    ## return the result
    rc[1]
}

best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv");
    
    ## Check that state and outcome are valid
    if(! state %in% levels(data$State)) {
        stop("invalid state");
    }
    outcomes <- c("heart attack", "heart failure", "pneumonia");
    if(! outcome %in% outcomes) {
        stop("invalid outcome");
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    subdata <- if(outcome == outcomes[1]) {
        subset(data, State == state, c(2, 11));
    }
    else if(outcome == outcomes[2]) {
        subset(data, State == state, c(2, 17));    
    }
    else {
        subset(data, State == state, c(2, 23));    
    }
    subdata[,2] <- as.numeric(as.character(subdata[,2]));
    subdata <- na.omit(subdata);
    subdata <- subdata[order(subdata[,2],subdata[,1]),];
    return(as.vector(subdata[1,1]));
}
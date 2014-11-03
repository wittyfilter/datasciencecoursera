rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
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
    rank_num <- if(num == "best") {
        1;
    }
    else if(num == "worst") {
        nrow(subdata);
    }
    else { 
        num;
    }
    if(rank_num > nrow(subdata)) {
        return(NA);
    }
    else {
        return(as.vector(subdata[rank_num,1]));
    }
}

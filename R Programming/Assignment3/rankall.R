rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv");
    
    ## Check that state and outcome are valid
    outcomes <- c("heart attack", "heart failure", "pneumonia");
    if(! outcome %in% outcomes) {
        stop("invalid outcome");
    }
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data <- if(outcome == outcomes[1]) {
        subset(data, select = c(2, 7, 11));
    }
    else if(outcome == outcomes[2]) {
        subset(data, select = c(2, 7, 17));    
    }
    else {
        subset(data, select = c(2, 7, 23));    
    }
    data[,3] <- as.numeric(as.character(data[,3]));
    data <- na.omit(data);
    hospital_name <- vector();
    i <- 1;
    for(state in levels(data$State)) {
        subdata <- subset(data , State == state);
        subdata <- subdata[order(subdata[,3],subdata[,1]),];
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
            hospital_name[i] <- NA;
        }
        else {
            hospital_name[i] <- (as.character(subdata[rank_num,1]));
        }
        i <- i + 1;
    }
    
    result <- data.frame(hospital = hospital_name, 
                         state = levels(data$State));
    result
}
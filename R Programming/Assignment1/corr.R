corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    output <- numeric()
    i <- 1
    
    for(file_name in dir(directory)){
        filename <- paste(directory, "/", file_name, sep = "")
        rawdata <- read.csv(filename)
        if(sum(complete.cases(rawdata)) > threshold){
            output[i] <- cor(rawdata["sulfate"], rawdata["nitrate"], use = "complete.obs")
            i <- i + 1
        }
    
    }
    
    output 
}
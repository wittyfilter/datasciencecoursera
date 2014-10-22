pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    totalsum <- 0
    totallength <- 0
    
    for(i in id){
        filename <- paste(directory, "/", sprintf("%.3d.csv",i), sep = "")
        rawdata <- read.csv(filename)
        datause <- rawdata[, pollutant][!is.na(rawdata[, pollutant])]
        totalsum <- totalsum + sum(datause)
        totallength <- totallength + length(datause)   
    }
    
    totalsum / totallength
}
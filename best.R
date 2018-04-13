
best <- function(state, outcome) {

## read the data  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## make a new dataframe with only the columns this function needs out to 46 or so.
## setting stringAsFactors to prevent R from automatically converting things to factors
  
  d <- as.data.frame(cbind( data[,2], data[,7], data[,11],data[,17],data[,23]), stringsAsFactors = FALSE)
  
## the coumns are unnamed as of now. so giving them appropriate names  
  colnames(d) <- c("hospital","state" ,"heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  
  if (!state %in% d[,"state"]){
    stop('Invalid State')
  }  
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('Invalid Outcome')
  }
  else {
     pos_st <- which(d[,"state"] == state)           ## returns all the row numbers where the state name in the dataframe has matched the state passed by the user to the function
     data_st <- d[pos_st,]                           ## extracting data from the respective positions obtained above into data_st 
     data_st_out <- as.numeric(data_st[,outcome])    ## extracting only the rows that fall under the required outcome. ( heart attack or heart failure or pneumonia)
     ##low <- min(data_st_out, na.rm = TRUE)             ## returns the minimum value of outcome. But we need hospital name which had that value...? hm. so dont do this.
     low_pos <- which.min(data_st_out)               ##Gives the position of the lowest value
     hosp <- data_st[low_pos,]                       ## Hospital details of lowest mortality rates for outcome in question
     hosp$hospital                                   ## returning the hospital name with the lowest 30-day death
  }
  
}
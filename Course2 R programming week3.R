#Programming Assignment 3 INSTRUCTIONS: Hospital Quality


setwd("X:/Chrome Download/rprog_data_ProgAssignment3-data")


#2. Finding the best hospital in a state
best <- function(state, outcome) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if (!state %in% data[,7]) {
        stop("invalid state")
    } 
    else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid outcome")
    } 
    else {
        col <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        data[, col[outcome]] <- as.numeric(data[, col[outcome]])
        splitstate <- split(data, data[,7])
        certainstate <- splitstate[[state]]
        certainstate[which.min(certainstate[, col[outcome]]), 2]
    }
}


#3. Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!state %in% data[,7]) {
        stop("invalid state")
    }
    else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
      stop("invalid outcome")
    }
    else {
        col <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        data[, col[outcome]] <- as.numeric(data[, col[outcome]])
        splitstate <- split(data, data[,7])
        certainstate <- splitstate[[state]]
        oderedstate <- certainstate[order(certainstate[, col[outcome]], certainstate[,2]),]
        naremove <- oderedstate[!is.na(oderedstate[, col[outcome]]),]
        ## Return hospital name in that state with the given rank 30-day death rate
        if (num == "best"){
            naremove[1 ,2]
        }
        else if (num == "worst"){
            naremove[nrow(naremove),2]
        }
        else if (as.numeric(num) > nrow(naremove)){
            NA
        }
        else {
            naremove[as.numeric(num) ,2]
        }
    }
}


#4. Ranking hospitals in all states
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}


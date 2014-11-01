best <- function(state,outcome) {
  ## Set Working Directory
  ## setwd("/Users/totomai/Documents/R Programming - Coursera/testfunctions/ProgrammingAssignment3")
  
  ## Read outcome data
  outcomeOfCare <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  UniqState <- unique(outcomeOfCare$State)
  ##UniqHosp <- unique(outcomeOfCare$Hospital.name)
  UniqOutcome <- c("heart attack","heart failure","pneumonia")
  
  
  ## Check that state and outcome are valid
  CheckState<- function(state){
                for (i in 1:length(UniqState))
                  ##print (c(i,UniqState[i]))
                  if (state == UniqState[i] ) { print(c("State matched!!", i))
                                                return (state)}
                    else if (i == length(UniqState)) stop("State is not Recognized")
                }
  
  CheckOutcome<- function(outcome){
    for (i in 1:length(UniqOutcome))
      if (outcome == UniqOutcome[i] ) { print(c("outcome matched!!", i))
                                    return (outcome)}
    else if (i == length(UniqOutcome)) stop("Outcome is not Recognized")
  }
   
  state<-CheckState(state)
  outcome<-CheckOutcome(outcome)
 
  
  ##if (state != )
  
  ## Return hospital name in that state with the lowest 30-day death rate
  ##if (outcome == "heart attack") minrate <- min(outcomeOfCare[,11]) 
  ##else if (outcome == "heart failure") minrate <- min(outcomeOfCare[,17])
 
  
}
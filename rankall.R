rankall <- function(outcome, num="best"){
  ## Read outcome data
  
  ## Set Working Directory
  ## setwd("/Users/totomai/Documents/R Programming - Coursera/testfunctions/ProgrammingAssignment3")
  
  ## Read Outcome Data
  outcomeOfCare <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  UniqState <- unique(outcomeOfCare$State)
  UniqOutcome <- c("heart attack","heart failure","pneumonia")
  dimnames(outcomeOfCare)[[2]][[11]]<-"heart.attack"  ##renaming long column name to be referenced later
  dimnames(outcomeOfCare)[[2]][[17]]<-"heart.failure"
  dimnames(outcomeOfCare)[[2]][[23]]<-"pneumonia"
  
  ## Check that state and outcome are valid
  CheckState<- function(state){
    for (i in 1:length(UniqState))
      if (state == UniqState[i] ) { ##print(c("State matched!!", i))
        return (state)}
    else if (i == length(UniqState)) return(FALSE)
  }
  CheckOutcome<- function(outcome){
    for (i in 1:length(UniqOutcome))
      if (outcome == UniqOutcome[i] ) { ##print(c("outcome matched!!", i))
        return (outcome)}
    else if (i == length(UniqOutcome)) return (FALSE) 
  }
  
  state<-CheckState(state)
  outcome<-CheckOutcome(outcome)
  if (state == FALSE) stop("invalid state")
  if (outcome == FALSE) stop("invalid outcome")
  
  
  ## Isolate per outcome
  if (outcome=="heart attack")  {
    col.outcome<-"heart.attack"
    
  }
  
  else if (outcome=="heart failure") {
    col.outcome<-"heart.failure"
                        }
  
  else if (outcome=="pneumonia") {
    col.outcome<-"pneumonia"
  }
  
  ## Clean data according to outcome only
  cases.ha<-complete.cases(as.numeric(outcomeOfCare[,col.outcome]))
  NoNA<-outcomeOfCare$Provider.Number[cases.ha]
  newOutcome<-outcomeOfCare[outcomeOfCare$Provider.Number %in% NoNA,]  ## no more NAs on heart.attack!
  
  ## For each state, find the hospital of the given rank
  
  ## Return a data frame with the hospital names and the abbreviated state names
}